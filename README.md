README
------

I like the idea of static site generators, but when I look at the
details of Jekyll and others, I'm a little put off.  Even not being a
Go aficiando, I liked playing with Hugo but the mismatches between its
current release, its documentation (e.g., archetypes) is maddening.  I
don't like second guessing someone's doc _or_ someone's code.  So, I
think it'll be fun to weld together a SSG in Common Lisp.  Why not?

Before SSG, I was all about Ghost.  I'd rather work "hands on" with my
own content and git behind the scenes, hence the SSG approach.

How hard can this be?  :-)  (famous last words)

**clog**  Common Lisp blOG.  Of course, it's really a static site
generator, but many people use them for blogging, hence the name.
But, truly, what's in a name?

Since my friend Patrick made a few comments, I've been looking at ways
to bundle this into a distributable executable.  I think we've got it,
using Roswell in this capacity.  I'm going to push it a little bit,
and develop under Windows 10 with CCL, instead of the more usual
Linux+SBCL.  FreeBSD was always my primary OS in the past (especially
when I was a LispWorks user), but these days I'm relegated to Windows;
sigh.  Besides, I think it'll be a nice portability push to make this
work under Windows.

We'll follow the Hugo model, so I'll credit it here as inspiration.
We'll have a content tree, a built in web server (I think I'll use
ningle, but definitely something on top of Clack), the ability to
run straight from memory for fast draft and development (especially
when people start needing to develop themes), and (of course) a
deliver-to-directory tree for a given site.

Out of the box, I think we'll support static site and blog
development.  I'm sure other people will add things as necessary.  To
that end, we'll need to document everything, so it's easy to extend.

Key Thoughts
------------

I think functional composition should be our special sauce.  It seems
the natural way to develop a site generator should be to think of a
site as a "something" that goes through successive refinements by many
small functions that only take a single bit of action.  I don't like
big bulky do-it-all frameworks, I'd rather see something that could
be used out of the box, something that a user could just add one or
two things here and there to existing functionality, and something
where an advanced user could replace huge chunks of functionality
themselves.

Themes should follow the same approach.  Gradual refinement of content
from original source (Markdown, org-mode, actual HTML, whatever)
into delivered media should be our goal.

The Common Source
-----------------

We'll support lots of different formats, it'll be easy.

Content files will start with metadata organized as a type (article,
blag entry, etc) followed by a simple property list.  After this top
s-exp, the rest of the file is plain old Markdown or whatever.  This
fits a Lisp engine well.  I bet many users won't even know—or
care—what a p-list is.  I'm imagining files with content like the
following.  The metadata starts off with a type (in fact, you can
imagine an invisible "type" at the beginning), followed by a simple
p-list containing anything.  It can override or refine metadata
provided in the current directory or even in the site configuration.
From **clog**'s point of view, the source of the metadata is
irrelevant, only that we get its priority (or hierarchy) correct.

    (article
        title "Great Googly Moogly"
        desc "Trudging across the tundra, mile after mile."
        date "2017-09-19 21:34")

    Well, the fur-trapper stood there, with his arms outstretched
    across the frozen white wasteland, trying to figure out what he
    was going to about his _deflicted_ eyes.
    ⋮

Of course, we'll have metadata configured for a site, metadata defined
at each point in a tree, and metadata in the content files.  For each
bit of content, they get combined in a way that lets files override or
otherwise enhance less-specific data.  This provides nice and easy
support for multiple kinds of sites, multiple and guest authors, and
so on.

Running It
----------

We should try to deliver a single executable `clog` like hugo does
with `hugo` or `hugo.exe` under Windows.  Everything else, including
theming and the like, should be specific to a content tree, and not to
a CLOG distribution.  A distribution should, ideally, be a simple
executable.  Downloadable, or buildable by the end user.

Ideally, I'd like to see something like

    clog [-s] [-...] [[srcdir] dstdir]

`-s` would tell clog to process `srcdir` and serve it from memory.
With memoization, serving from memory ought to be very easy to
adapt from site generation.

If neither `srcdir` nor `dstdir` are supplied, then **clog** can
only do ephemeral things (serving, validating, maybe other kinds
of testing?)

If both are supplied, `srcdir` names the source hierarchy where the
content and theme is provided, and `dstdir` is the new directory
hierarchy to write for the site.

When `srcdir` is not supplied, the current directory is taken to be
the source tree.

Source Tree
-----------

    srcdir/
    srcdir/config
    srcdir/content/
    srcdir/static/
    srcdir/themes/

**clog** shouldn't care much what a file is named.  If it exists in
the content tree, it is parsed and processed.  If it exists in the
static tree, it appears in the site as-is.  If it exists in the themes
tree, it is used locally but not copied to the site directory.

We'll probably walk the `static` tree, just to know what exists?
Meh.  So far, walking the `content` tree and processing that was
more than sufficient.

Internal Representation
-----------------------

For a SSG, I think p-lists are the right answer.  We want something
simple, extensible, and not easily anticipated.  Sure, just about
_anything_ is faster than a p-list.  We could use a hash for each bit
of content, or if we really have to go fast, a struct.  But I'd rather
keep the essential flexibility of a p-list until I actually see that
performance needs to be addressed.

So, eventually, we have an internal representation of a site that
looks like

    ((path #P"…" title "…" desc "…" date "…" type article etc etc)
     (path #P"…" title "…" desc "…" date "…" type blog etc etc)
     ⋮
     (path #P"…" title "…" desc "…" date "…" type blog etc etc))
