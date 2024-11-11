.. _code-generators:

GHC Backends
============

.. index::
   single: GHC backends
   single: GHC code generators

GHC supports multiple backend code generators. This is the part of the
compiler responsible for taking the last intermediate representation
that GHC uses (a form called Cmm that is a simple, C like language) and
compiling it to executable code. The backends that GHC support are
described below.

.. _native-code-gen:

Native Code Generator (``-fasm``)
---------------------------------

.. index::
   single: native code generator

The default backend for GHC. It is a native code generator, compiling
Cmm all the way to assembly code. It is the fastest backend and
generally produces good performance code. It has the best support for
compiling shared libraries. Select it with the ``-fasm`` flag.

.. _llvm-code-gen:

LLVM Code Generator (``-fllvm``)
--------------------------------

.. index::
   single: LLVM code generator

This is an alternative backend that uses the `LLVM <https://llvm.org>`__
compiler to produce executable code. It generally produces code with
performance as good as the native code generator but for some cases can
produce much faster code. This is especially true for numeric, array
heavy code using packages like vector. The penalty is a significant
increase in compilation times. Select the LLVM backend with the
:ghc-flag:`-fllvm` flag.

You must install and have LLVM available on your ``PATH`` for the LLVM code
generator to work. Specifically GHC needs to be able to call the ``opt``
and ``llc`` tools. Secondly, if you are running Mac OS X with LLVM 3.0
or greater then you also need the `Clang C
compiler <https://clang.llvm.org>`__ compiler available on your ``PATH``.

.. note::

    Note that this GHC release expects an LLVM version in the |llvm-version-min|
    up to |llvm-version-max| (not inclusive) release series.

To install LLVM and Clang:

-  *Linux*: Use your package management tool.

-  *Mac OS X*: Clang is included by default on recent OS X machines when
   Xcode is installed (from 10.6 and later). LLVM is not included.
   In order to use the LLVM based code generator, you should install the
   `Homebrew <https://github.com/Homebrew/brew>`__ package manager for
   OS X. Alternatively you can download binaries for LLVM and Clang from
   `here <https://llvm.org/releases/download.html>`__.

-  *Windows*: You should download binaries for LLVM and clang from
   `here <https://llvm.org/releases/download.html>`__.

.. _c-code-gen:

C Code Generator (``-fvia-C``)
------------------------------

.. index::
   single: C code generator
   single: -fvia-C

.. ghc-flag:: -fvia-C
    :shortdesc: use the C code generator
    :type: dynamic
    :category: warnings

    Use the C code generator. Only supposed in unregisterised GHC builds.

This is the oldest code generator in GHC and is generally not included
any more having been deprecated around GHC 7.0. Select it with the
:ghc-flag:`-fvia-C` flag.

The C code generator is only supported when GHC is built in
unregisterised mode, a mode where GHC produces "portable" C code as
output to facilitate porting GHC itself to a new platform. This mode
produces much slower code though so it's unlikely your version of GHC
was built this way. If it has then the native code generator probably
won't be available. You can check this information by calling
``ghc --info`` (see :ghc-flag:`--info`).

.. _javascript-code-gen:

JavaScript Code Generator
------------------------------

.. index::
   single: JavaScript code generator

This is an alternative code generator included in GHC 9.6 and above. It
generates `ECMA-262 <https://tc39.es/ecma262/>`_ compliant JavaScript and is
included as a technical preview. At time of writing, it is being actively
developed but is not suitable for serious projects and production environments.
The JavaScript backend is not distributed in the GHC bindist and requires a
manual build. See `building the JavaScript backend
<https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building>`_ page
on the GHC wiki for build instructions.

A JavaScript cross-compiling GHC produces an executable script, and a directory
of the same name suffixed with ``.jsexe``. For example, compiling a file named
``Foo.hs`` will produce an executable script ``Foo`` and a ``Foo.jsexe``
directory. The script is a thin wrapper that calls `Node.js
<https://nodejs.org/en/>`_ on the payload of the compiled Haskell code and can
be run in the usual way, e.g., ``./Foo``, as long as ``node`` is in your
environment . The actual payload is in ``<ModuleName>.jsexe/all.js``, for
example ``Foo.jsexe/all.js``. This file is the Haskell program cross-compiled to
JavaScript *concrete syntax* and can be wrapped in a ``<script>`` HTML tag. For
a breakdown of the rest of the build artifacts see the `compiler output
<https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building#compiler-output-and-build-artifacts>`_
section in the wiki.

.. _unreg:

Unregisterised compilation
--------------------------

.. index::
   single: unregisterised compilation

The term "unregisterised" really means "compile via vanilla C",
disabling some of the platform-specific tricks that GHC normally uses to
make programs go faster. When compiling unregisterised, GHC simply
generates a C file which is compiled via gcc.

When GHC is built in unregisterised mode only the C code generator is
available.  Neither the LLVM nor native code generator can be used by an
unregisterised build.

Unregisterised compilation can be useful when porting GHC to a new
machine, since it reduces the prerequisite tools to ``gcc``, ``as``, and
``ld`` and nothing more, and furthermore the amount of platform-specific
code that needs to be written in order to get unregisterised compilation
going is usually fairly small.

Unregisterised compilation cannot be selected at compile-time; you have
to build GHC with the appropriate options set. Consult the GHC Building
Guide for details.

You can check if your GHC is unregisterised by calling
``ghc --print-unregisterised`` (see :ghc-flag:`--print-unregisterised`) or
``ghc --info`` (see :ghc-flag:`--info`).
