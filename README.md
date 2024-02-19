# Project Norebo Plus

See below for original readme. This fork has not yet been updated with changes I've made in my copy over time.

I haven't made that many changes, either. Just a couple. However, I have always felt that this particular
way of using oberon (I use it like 'nw ORP.Compile Foo.Mod' for example, where 'nw' is a shell script
wrapper around the norebo runtime) has real potential, in a couple of dimensions.

One is: the idea of communicating sequential processes, where each process is a node in a network of oberon
RISC5 machines (virtual or real). To play with that idea would be relatively simple, given a norebo, and
shell scripts.

But more so: writing "real-world" Oberon modules, things that can actually be used in the modern world. I'm
not entirely sure this is reasonable, because Oberon (meaning Oberon-07) is rather under-powered, relative
to something like C#. I might be understating the gulf between them.

On the other hand: Oberon is a human-sized language, whereas a C# is going to take some real study. Python is
a good choice, though, too, sort of - and obviously way more popular. 

But I mean, Oberon-07 is a formal, computer science sort of language, a distillation
of practical and mathematical and minimal. Python is executable pseudo-code, basically. One builds a Python using
an Oberon, not the other way around. As such, is more like a C or an Ada or a Zig, all three of which are considerably
harder to understand and use than Oberon.

Of course, the RISC5 VM in this code is slow, relative to what a compiled version of the same code would do. But one
of the changes I made was to the VM so that if it is compiled with full optimizations, the resulting code is really
quite speedy, given that it is interpreting the RISC5 instruction streams, which is itself a simple instruction set
and not inherently speedy (not like an ARM would be).

One of the things I was thinking would be neat to do would be to have the compiler's output, which is for RISC5, be
transliterated into ARM aarch64 instructions - there is presumably an onto mapping from RISC5 to aarch64 instructions,
both of which are 32-bit long. I might see if Copilot can help me generate (Oberon) code for that.

And, finally, this is here just in case. I'd hate to see this code disappear from the internet.

---

# Project Norebo

Norebo is a hack to run some _Project Oberon 2013_ software on the
Unix command line. Programs that use the GUI obviously won't work, but
e.g. the compiler runs.

I probably won't be maintaining this project, so feel free to fork
if you want to develop it further.

## Contents

* `Runtime/` RISC5 emulator and operating system interface.
* `Oberon/` Unmodified source code from Project Oberon 2013.
* `Norebo/` Norebo-specific and new modules.
* `Bootstrap/` Pre-compiled modules to bootstrap Norebo.
* `build.sh` Script to rebuild Norebo. See Norebo in action.

## PO2013 image build tools

This repository also contains tools to build fresh PO2013 filesystem
images.  Use it like so:

    ./fetch-sources.py upstream
    ./build-image.py upstream

...where `upstream` is the name of the directory where the sources
should live.  (Replace it with the name of your choice.) This will
download the project sources, compile them, create runnable disk image
`build/Oberon.dsk`.  The CSV build manifest controls which set of
files should define the resulting system.  The disk image can be run
on the [Project Oberon RISC emulator].

Supporting Oberon modules are stored in `Norebo`: a virtual file
system (`VDiskUtil`/`VFile`) and a static linker for the Inner Core.
All this is based on code from PO2013.

## File handling

New files are always created in the current directory. Old files are
first looked up in the current directory and if they are not found,
they are searched for in the path defined by the `OBERON_PATH`
environment variable. Files found via `OBERON_PATH` are always opened
read-only.

## Bugs

Probably many.

Files are not integrated with the garbage collector. If you don't
close a file, it will remain open until Norebo exits.

Most runtime errors do not print a diagnostic message. Here's a table
of exit codes:

 Exit code | Meaning
----------:|:------------------------------
      1..7 | possibly a Modules error
         5 | (also) unknown command
       101 | array index out of range
       102 | type guard failure
       103 | array or string copy overflow
       104 | access via NIL pointer
       105 | illegal procedure call
       106 | integer division by zero
       107 | assertion violated

[Project Oberon RISC emulator]: https://github.com/pdewacht/oberon-risc-emu
