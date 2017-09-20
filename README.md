README
------

I like aspects of Hugo, but other parts of it are driving me nuts.
So, I'll borrow some ideas, credit them when they come from Hugo,
and write the rest myself.

Running It
----------

We'll deliver a single executable, clog, like hugo does with hugo.

    clog srcdir dstdir

Reads a tree in srcdir, generating a new tree at dstdir.  If dstdir
already exits, clog should exit with an error.  dstdir should be
self-contained and complete, within reason.

    cd ...
    clog dstdir

Enters a source tree, and generates a new site at dstdir.

    cd ...
    clog

Enters a source tree, generating a new site at a destination
directory named in a configuration file.

Source Tree
-----------

A directory hierarchy describing a site.

    srcdir/
    srcdir/config

A file containing a description of the srcdir.

    srcdir/themes/

A theme is a directory; certain files have to exist in certain places
in a theme; the rest of the subtree is organized however the theme
needs it to bo.

    srcdir/content/

Files are organized in any way under the content directory.
Subdirectories and hierarchies are completely optional.  They are
tracked, of course, but clog doesn't care about them.  You could put
all the files of your site right in content, or you could organize
them by type, or by date, or whatever.

Source Files
------------

Each file starts with one S-expression that contains all the metadata
for the file, overriding defaults that might have been set up in the
config files (authors, etc).  After the expression, the rest of the
file is in Markdown (or some other) format.

You can indent the initial metadata expression as a code block, if
that helps your editor behave; clog won't care.

Each file resembles this:

    (article
        title "Great Googly Moogly"
        desc "Trudging across the tundra, mile after mile."
        date "2017-09-19 21:34:42")

    Well, the fur-trapper stood there, with his arms outstretched
    across the frozen white wasteland, trying to figure out what he
    was going to about his _deflicted_ eyes.

Other metadata can be present, too, of course.  For example, an author
might be specified in the site's config file, but you might have an
article written by a guest.  In that case, you'd put the usual name into
the config file, let all the other files inherit it, but in this one
special bit of content, you'd add an `author` line to its metadata.
