/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * Functions called from outside the GC need to be separate from GC.c,
 * because GC.c is compiled with register variable(s).
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "GC.h"
#include "CheckUnload.h"
#include "Storage.h"
#include "Compact.h"
#include "Task.h"
#include "Capability.h"
#include "Trace.h"
#include "Schedule.h"
// DO NOT include "GCTDecl.h", we don't want the register variable

/* -----------------------------------------------------------------------------
   isAlive determines whether the given closure is still alive (after
   a garbage collection) or not.  It returns the new address of the
   closure if it is alive, or NULL otherwise.

   NOTE: Use it before compaction only!
         It untags and (if needed) retags pointers to closures.
   -------------------------------------------------------------------------- */

StgClosure *
isAlive(StgClosure *p)
{
  const StgInfoTable *info;
  bdescr *bd;
  StgWord tag;
  StgClosure *q;

  while (1) {
    /* The tag and the pointer are split, to be merged later when needed. */
    tag = GET_CLOSURE_TAG(p);
    q = UNTAG_CLOSURE(p);

    ASSERT(LOOKS_LIKE_CLOSURE_PTR(q));

    // ignore static closures
    //
    // ToDo: This means we never look through IND_STATIC, which means
    // isRetainer needs to handle the IND_STATIC case rather than
    // raising an error.
    //
    // ToDo: for static closures, check the static link field.
    // Problem here is that we sometimes don't set the link field, eg.
    // for static closures with an empty SRT or CONSTR_NOCAFs.
    //
    if (!HEAP_ALLOCED_GC(q)) {
        return p;
    }

    // ignore closures in generations that we're not collecting.
    bd = Bdescr((P_)q);

    // isAlive is used when scavenging moving generations, before the mark
    // phase. Because we don't know alive-ness of objects before the mark phase
    // we have to conservatively treat objects in the non-moving generation as
    // alive here.
    if (bd->flags & BF_NONMOVING) {
        return p;
    }

    // if it's a pointer into to-space, then we're done
    if (bd->flags & BF_EVACUATED) {
        return p;
    }

    // large objects use the evacuated flag
    if (bd->flags & BF_LARGE) {
        return NULL;
    }

    // check the mark bit for compacted generations
    if ((bd->flags & BF_MARKED) && is_marked((P_)q,bd)) {
        return p;
    }

    info = RELAXED_LOAD(&q->header.info);

    if (IS_FORWARDING_PTR(info)) {
        // alive!
        return TAG_CLOSURE(tag,(StgClosure*)UN_FORWARDING_PTR(info));
    }

    ACQUIRE_FENCE_ON(&q->header.info);
    info = INFO_PTR_TO_STRUCT(info);

    switch (info->type) {

    case IND:
    case IND_STATIC:
      // follow indirections
      p = ((StgInd *)q)->indirectee;
      continue;

    case BLACKHOLE:
        p = ((StgInd*)q)->indirectee;
        if (GET_CLOSURE_TAG(p) != 0) {
            continue;
        } else {
            return NULL;
        }

    default:
      // dead.
      return NULL;
    }
  }
}

/* -----------------------------------------------------------------------------
   Reverting CAFs
   -------------------------------------------------------------------------- */

void
revertCAFs( void )
{
    StgIndStatic *c = revertible_caf_list;

    while (c != (StgIndStatic *) END_OF_CAF_LIST) {
        c = (StgIndStatic *)UNTAG_STATIC_LIST_PTR(c);
        StgIndStatic *next = (StgIndStatic *) c->static_link;

        SET_INFO((StgClosure *)c, c->saved_info);
        c->saved_info = NULL;
        // We must reset static_link lest the major GC finds that
        // static_flag==3 and will consequently ignore references
        // into code that we are trying to unload. This would result
        // in reachable object code being unloaded prematurely.
        // See #16842.
        c->static_link = NULL;
        c = next;
    }
    revertible_caf_list = (StgIndStatic*)END_OF_CAF_LIST;
}

void
markCAFs (evac_fn evac, void *user)
{
    /* N.B. We must both ensure that the indirectee is
     * evacuated and that we let the linker know that the CAF
     * itself is still reachable, lest it be collected (see
     * #20649).
     */
    for (StgIndStatic *c = dyn_caf_list;
         ((StgWord) c | STATIC_FLAG_LIST) != (StgWord)END_OF_CAF_LIST;
         c = (StgIndStatic *)c->static_link)
    {
        c = (StgIndStatic *)UNTAG_STATIC_LIST_PTR(c);
        evac(user, &c->indirectee);
        // See Note [Object unloading] in CheckUnload.c
        if (unload_mark_needed) markObjectCode(c);
    }

    for (StgIndStatic *c = revertible_caf_list;
         ((StgWord) c | STATIC_FLAG_LIST) != (StgWord)END_OF_CAF_LIST;
         c = (StgIndStatic *)c->static_link)
    {
        c = (StgIndStatic *)UNTAG_STATIC_LIST_PTR(c);
        evac(user, &c->indirectee);
        // See Note [Object unloading] in CheckUnload.c
        if (unload_mark_needed) markObjectCode(c);
    }
}
