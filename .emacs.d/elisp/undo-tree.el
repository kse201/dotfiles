


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
 "http://www.w3.org/TR/html4/loose.dtd">

<html>
<head>
  <title>Toby 'qubit' Cubitt - Emacs Code</title>
  <meta name="Author" content="Toby Cubitt">

  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <meta http-equiv="Content-Script-Type" content="text/javascript">

  <link rel="stylesheet" title="Black keys" type="text/css" media="screen"
   href="black-keys.css">
</head>



<body bgcolor="black" text="white">

<div class="header"></div>



<div id="content">
<div class="page0">
<div class="page1">
<div class="page2">
<div class="main">


<ul class="toc">
  <li>
    <a href="#predictive">Predictive</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
    <ul>
      <li>
	<a href="#predictive-docs">Docs</a>
      </li>
      <li><a href="#predictive-download">Download</a></li>
      <li><a href="#predictive-bugs">Bugs</a></li>
    </ul>
  </li>
  <li>
    <a href="#undo-tree">Undo-Tree</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
    <ul>
      <li>
	<a href="#undo-tree-docs">Docs</a>
      </li>
      <li><a href="#undo-tree-download">Download</a></li>
      <li><a href="#undo-tree-bugs">Bugs</a></li>
    </ul>
  </li>
  <li>
    <a href="#completion">CompletionUI</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
    <ul>
      <li>
	<a href="#completion-docs">Docs</a>
      </li>
      <li><a href="#completion-download">Download</a></li>
    </ul>
  </li>
  <li>
    <a href="#overlay">Auto Overlays</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
    <ul>
      <li>
	<a href="#overlay-docs">Docs</a>
      </li>
      <li><a href="#overlay-download">Download</a></li>
    </ul>
  </li>
  <li>
    <a href="#structures">Structures</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
    <ul>
      <li>
	<a href="#structures-docs">Docs</a>
      </li>
      <li><a href="#structures-download">Download</a></li>
    </ul>
  </li>
  <li>
    <a href="#misc">Misc</a>
    <img class="corner" src="images/backgrounds/corner10.gif">
  </li>
</ul>




<h1>Emacs Code</h1>
<img class="corner" src="images/backgrounds/corner20.gif">

<p>This page contains the Emacs lisp packages I develop and maintain
(see below for more detailed descriptions):</p>

<p>
<ul>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/predictive-0.23.13.tar.gz">      <img src="images/icons/tar.gif"> Predictive Completion package</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.5.1.el">      <img src="images/icons/elisp.gif"> Undo-Tree package</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/completion-ui-0.11.11.tar.gz">      <img src="images/icons/tar.gif"> Completion UI package</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/auto-overlays-0.10.7.tar.gz">      <img src="images/icons/tar.gif"> Automatic Overlays package</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/heap-0.3.el">      <img src="images/icons/elisp.gif"> heap.el (heap data structure)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/queue-0.1.el">      <img src="images/icons/elisp.gif"> queue.el (queue data structure)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/avl-tree-0.2.el">      <img src="images/icons/elisp.gif"> avl-tree.el (AVL tree data structure)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/tNFA-0.1.1.el">      <img src="images/icons/elisp.gif"> tNFA.el (tagged non-deterministic
      finite state automata)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/trie-0.2.5.el">      <img src="images/icons/elisp.gif"> trie.el (trie data structure)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/dict-tree-0.12.7.el">      <img src="images/icons/elisp.gif"> dict-tree.el (dictionary data structure)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=emacs-misc/show-point-mode-0.2.el">      <img src="images/icons/elisp.gif"> show-point-mode.el (show point in mode line)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=emacs-misc/wc-mode-0.1.el">      <img src="images/icons/elisp.gif"> wc-mode.el (show wc output in mode line)</a>
    </span>
  </li>
</ul>
</p>





<h2 id="predictive">Predictive Completion</h2>
<p>The Emacs Predictive Completion package adds a new minor-mode to the
GNU Emacs editor. When enabled, predictive mode exploits the redundancy
inherent in languages in order to complete words you are typing before
you've finished typing them (somewhat like the IntelliSense feature in
some IDEs). It is highly customisable, and works happily alongside other
Emacs major modes. See the <a href="#docs">documentation</a> for more
details.</p>

<p>Predictive mode only works under GNU Emacs, not under XEmacs. It may
be possible to get it to work under XEmacs with a modicum of work. (At
the very least, the overlay compatibility package would be required.) If
you're interested in attempting this, then I'm happy to answer questions,
but I have no plans to do it myself.</p>


<h3 id="predictive-docs">Documentation</h3>
<p>Full documentation is available in a variety of formats, for
download and viewing online.</p>

<p>If you're interested in using predictive mode, you should read the
extensive Predictive User Manual.</p>

<p>The Predictive Programmer Manual contains low-level information for
people who want to write their own lisp code to enhance predictive mode,
for instance to make predictive completion work in other major
modes. However, there is one reason why you might not want to read the
programmer manual: it doesn't exist yet. Some of the information that
would be in the programmer manual, were it to exist, is described in the
<a href="#overlay-docs">auto-overlay manual</a> (which does exist!), and
the <a href="#completion-ui-manual">Completion-UI manual</a> would
contain further relevant information, were <emph>it</emph> to exist!</p>

<h4 id="predictive-user-manual">Predictive User Manual</h4>
<ul class="files">
  <li>
    <span class="files"><a
    href="predictive/predictive-user-manual/html/index.php"><img
    src="images/icons/link.gif"> Web documentation</a></span>
  </li>
  <li>
    <span class="files"><a
    href="download.php?file=predictive/predictive-user-manual/predictive-user-manual-html.tar.gz"><img
    src="images/icons/html.gif"> HTML download (gzipped tar
    file)</a></span>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-user-manual/predictive-user-manual.info.gz"><img
    src="images/icons/info.gif"> Info document (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-user-manual/predictive-user-manual.txt.gz"><img
    src="images/icons/txt.gif"> ASCII text (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-user-manual/predictive-user-manual.pdf"><img
    src="images/icons/pdf.gif"> PDF file</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-user-manual/predictive-user-manual.ps.gz"><img
    src="images/icons/ps.gif"> Postscript file (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-user-manual/predictive-user-manual.texinfo"><img
    src="images/icons/texi.gif"> Texinfo source </a></span>
  </li>
</ul>


<h4 id="predictive-programmer-manual">Predictive Programmer Manual</h4>
<img src="images/men_at_work.png">
<!--
<div class="subheading">(incomplete)</div>
<ul class="files">
  <li>
    <span class="files"><a
    href="predictive/predictive-programmer-manual/html/index.php"><img
    src="images/icons/link.gif"> Web documentation</a></span>
  </li>
  <li>
    <span class="files"><a
    href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual-html.tar.gz"><img
    src="images/icons/html.gif"> HTML download (gzipped tar
    file)</a></span>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual.info.gz"><img
    src="images/icons/info.gif"> Info document (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual.txt.gz"><img
    src="images/icons/txt.gif"> ASCII text (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual.pdf"><img
    src="images/icons/pdf.gif"> PDF file</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual.ps.gz"><img
    src="images/icons/ps.gif"> Postscript file (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/predictive-programmer-manual/predictive-programmer-manual.texinfo"><img
    src="images/icons/texi.gif"> Texinfo source </a></span>
  </li>
</ul>
-->


<h4 id="other">Other sources of information</h4>
<p>The <a href="http://www.emacswiki.org/emacs/PredictiveMode">Predictive
Mode</a> page on the <a href="http://www.emacswiki.org">Emacs Wiki</a>
contains various useful pieces of information about the Predictive
Completion package. (And if you don't find the information you want, you
can always add it yourself and then read it.)</p>

<p>Mathias Dahl has also posted a <a
href="http://klibb.com/cgi-bin/wiki.pl/Emacs_Blog_2007-01-20">nice
tutorial</a> about using predictive mode on his blog.</p>



<h3 id="predictive-download">Download and Installation</h3>

<p>The current release of the Predictive Completion package is version
0.23.13 (released April 2012). It's still under active development, so don't forget to check back
here for updates every so often. If you want to live on the bleeding
edge, the latest "development" version of the Predictive package is
hosted in a git repository.
<!-- (A few older versions are also available below, but are no longer
supported.) -->
</p>

<p>
<ul>
  <li>
    <span class="files">
      Git repository: <code>http://www.dr-qubit.org/git/predictive.git</code>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/predictive-0.23.13.tar.gz">      <img src="images/icons/tar.gif"> Current version (0.23.13)</a>
    </span>
  </li>
<!--
  <li>
    <span class="files">
      <a href="download.php?file=predictive/predictive-0.21.tar.gz">      <img src="images/icons/tar.gif"> Version 0.21</a>
    </span>
  </li>
-->
</ul>
</p>

<p>Note that the git repository URL is a git repository, <em>not</em> a
web-site. You <em>cannot</em> view it in a web browser. To grab the
latest development version, clone the repository using something
like:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>git clone http://www.dr-qubit.org/git/predictive.git</code></p>

<p>Whether you downloaded the package or cloned the git repository, you
need to perform some further steps in order to install the Predictive
Completion package on your system. See
the <a href="predictive/predictive-user-manual/html/Obtaining-and-Installing.html">Obtaining
and Installing</a> section of the manual and the <code>INSTALL</code>
file included in the package for installation instructions.</p>



<h3 id="predictive-bugs">Bugs and Feature Requests</h3>

<p>Bugs, feature requests, and any other feedback should be addressed
to <a href="mailto:toby-predictive@dr-qubit.org">toby-predictive@dr-qubit.org</a>.
(Note that this address is protected by the TMDA spam-reduction
system. The first time you send an email, you will have
to <a href="email.php">verify your address</a> by replying to a
confirmation message.)</p>

<p>If you have code you would like to contribute to the Predictive
package, either send a patch against the latest development version
to <a href="mailto:toby-predictive@dr-qubit.org">toby-predictive@dr-qubit.org</a>,
or, better still use git, rebase your work against the latest git master,
and let me know where to find your clone of the Predictive repository so
that I can pull your changes.</p>

<p>A list
of <a href="predictive/predictive-user-manual/html/Bugs-and-Future-Improvements.html">known
bugs and feature suggestions</a> is included at the end of the user
manual.</p>





<h2 id="undo-tree">Undo Tree</h2>

<p>Emacs has a powerful undo system. Unlike the standard undo/redo system
in most software, it allows you to recover <em>any</em> past state of a
buffer (whereas the standard undo/redo system can lose past states as
soon as you redo). However, this power comes at a price: many people find
Emacs' undo system confusing and difficult to use, spawning a number of
packages that replace it with the less powerful but more intuitive
undo/redo system. (See
the <a href="http://www.emacswiki.org/emacs/CategoryUndo">Emacs
Wiki</a>.)</p>

<p>Both the loss of data with standard undo/redo, and the confusion of
Emacs' undo, stem from trying to treat undo history as a linear sequence
of changes. It's not. The <code>undo-tree-mode</code> provided by this
package replaces Emacs' undo system with a system that treats undo
history as what it is: a branching tree of changes. This simple idea
allows the more intuitive behaviour of the standard undo/redo system to
be combined with the power of never losing any history. An added side
bonus is that undo history can in some cases be stored more efficiently,
allowing more changes to accumulate before Emacs starts discarding
history.</p>

<p>It gets better. You don't have to imagine the undo tree,
because <code>undo-tree-mode</code> includes an undo-tree visualizer
which draws it for you, and lets you browse around the undo history.</p>

<p>The only downside to this more advanced yet simpler undo system is
that it was inspired by Vim. But, after all, most successful religions
steal the best ideas from their competitors!</p>



<h3 id="undo-tree-docs">Documentation</h3>

<p>Details of the <code>undo-tree-mode</code> commands and key bindings
can be found in the Commentary section at the top of
the <code>undo-tree.el</code> file itself, along with an extended
explanation (with diagrams!) of the differences between
the <code>undo-tree-mode</code>, standard Emacs' undo, and undo/redo
systems.</p>

<p>The commands themselves are all documented using the standard,
built-in Emacs documentation features. Customization options can be found
under the <code>undo-tree</code> customization group.</p>



<h3 id="undo-tree-download">Download and Installation</h3>

<p>The current "stable" release of the Undo-Tree package is version
0.5.1 (released May 2012). It's still under active development, so don't forget to check back
here for updates every so often. (A few older versions are also available
below, but are no longer supported.)</p>

<p>In recent versions of Emacs (&ge;24.1), you can also install the
Undo-Tree package from within Emacs itself,
via <a href="http://elpa.gnu.org/">GNU ELPA</a>. Use <code>M-x
list-packages</code> and take it from there. (The ELPA version
occasionally lags slightly behind the latest version available here.)</p>

<p>If you want to live on the bleeding edge, the latest "development"
version of the Undo-Tree package is hosted in a git repository.</p>

<p>
<ul>
  <li>
    <span class="files">
      Git repository: <code>http://www.dr-qubit.org/git/undo-tree.git</code>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.5.1.el">      <img src="images/icons/elisp.gif"> Current version (0.5.1)</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.4.el">      <img src="images/icons/elisp.gif"> Version 0.4</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.3.4.el">      <img src="images/icons/elisp.gif"> Version 0.3.4</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.2.1.el">      <img src="images/icons/elisp.gif"> Version 0.2.1</a>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=undo-tree/undo-tree-0.1.7.el">      <img src="images/icons/elisp.gif"> Version 0.1.7</a>
    </span>
  </li>
</ul>
</p>

<p>Note that the git repository URL is a git repository, <em>not</em> a
web-site. You <em>cannot</em> view it in a web browser. To grab the
latest development version, clone the repository using something
like:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>git clone http://www.dr-qubit.org/git/undo-tree.git</code></p>


<p>Regardless of where you obtained it, to install the Undo-Tree package
simply save the <code>undo-tree.el</code> file to a directory in your
Emacs <code>load-path</code>, and add the line:</p>

<p>&nbsp;&nbsp;&nbsp;<code>(require 'undo-tree)</code></p>

<p>to your <code>.emacs</code> file. "<code>M-x undo-tree-mode</code>"
will then enable <code>undo-tree-mode</code> in the current buffer.</p>

If you want to replace the standard Emacs' undo system with the Undo-Tree
system in all normal buffers, you can enable <code>undo-tree-mode</code>
globally by adding:

<p>&nbsp;&nbsp;&nbsp;<code>(global-undo-tree-mode)</code></p>

<p>to your <code>.emacs</code> file.</p>



<h3 id="undo-tree-bugs">Bugs, Feature Requests, and Contributing</h3>

<p>Bugs, feature requests, and any other feedback should be addressed
to <a href="mailto:toby-undo-tree@dr-qubit.org">toby-undo-tree@dr-qubit.org</a>.
(Note that this address is protected by the TMDA spam-reduction
system. The first time you send an email, you will have
to <a href="email.php">verify your address</a> by replying to a
confirmation message.)</p>

<p>If you have code you would like to contribute to Undo-Tree, either
send a patch against the latest development version
to <a href="mailto:toby-undo-tree@dr-qubit.org">toby-undo-tree@dr-qubit.org</a>.
Or, better still, use git, rebase your work against the latest git
master, and let me know where to find your clone of the Undo-Tree
repository so that I can pull your changes.</p>






<h2 id="completion">Completion User Interface</h2>

<p>The Completion User Interface package is a library that implements
user-interfaces for in-buffer completion.</p>

<p>Typically, a lot of code in packages providing some kind of text
completion deals with the user interface. The goal of Completion-UI is to
be the swiss-army knife of in-buffer completion user-interfaces, which
any source of completions can plug in to, thus freeing completion package
writers to concentrate on the task of finding the completions in the
first place. In fact, Completion-UI is even better than a swiss-army
knife, because it's also extensible: it's easy to add new
user-interfaces, as well as new completion sources.</p>

<p>Various completion user-interfaces and commands are provided, which
can be separately enabled, disabled and tweaked by the Emacs user via the
usual bewildering array of customization variables:</p>

<dl>
  <dt>Dynamic completion</dt>
  <dd>
    Provisionally inserts the best completion candidate in the buffer,
    highlighting the completed portion.
  </dd>

  <dt>Completion hotkeys</dt>
  <dd>
    Single-key selection of a completion.
  </dd>

  <dt>Cycling</dt>
  <dd>
    Cycle through completion candidates.
  </dd>

  <dt>Tab-completion</dt>
  <dd>
    "Traditional" expansion to longest common substring.
  </dd>

  <dt>Echo area</dt>
  <dd>
    Display a list of completion candidates in the echo-area.
  </dd>

  <dt>Tooltip</dt>
  <dd>
    Display a list of completion candidates in a tool-tip located below
    the point.
  </dd>

  <dt>Pop-up frame</dt>
  <dd>
    Allow completion candidates to be selected from a pop-up frame
    displayed below the point.
  </dd>

  <dt>Completion menu</dt>
  <dd>
    Allow completion candidates to be selected from a drop-down menu
    located below the point.
  </dd>

  <dt>Completion browser</dt>
  <dd>
    Allow completion candidates to be selected from a hierarchically
    organised stack-of-cards menu structure.
  </dd>

  <dt>auto-completion-mode</dt>
  <dd>
    Automatically complete words as you type.
  </dd>
</dl>

<p>The philosophy of Completion-UI is that customization of the
user-interface should be left up to <emph>users</emph>. They know what
they want better than you do! And by providing a universal user-interface
that can be used by all completion packages, Completion-UI lets users
customize their in-buffer completion user interface once-and-for-all to
suit their tastes, rather than having to learn how to customize each new
package separately.</p>

<p>Completion-UI comes with built-in support
for <code>dabbrevs</code>, <code>etags</code>, Elisp and file-name
completion, as well as supporting (if
installed) <a href="http://cedet.sourceforge.net/">CEDET's Semantic</a>
completion, <code>nxml-mode</code>, and
the <a href="#predictive">predictive completion</a> package.</p>

<p>For each source of completions, Completion-UI provides an interactive
command that completes the word next to the point using that source, e.g:
<code>complete-dabbrev</code> to complete using
dabbrevs, <code>complete-etags</code> to complete using
etags, <code>complete-elisp</code> to complete Elisp
symbols, <code>complete-files</code> to complete file
names, <code>complete-semantic</code> to use Semantic
completion, <code>complete-nxml</code>
and <code>complete-predictive</code> to use nxml-mode and predictive-mode
completion, respectively.</p>

<p>The <code>complete-&lt;name&gt;</code> commands are not bound to any
key by default. As with any Emacs command, you can run them via
"<code>M-x complete-&lt;name&gt;</code>", or you can bind them to keys,
either globally or in a minor-mode keymap. E.g. to globally bind "M-/"
to <code>complete-dabbrev</code>, you would put the following line in
your .emacs file:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>(global-set-key [?\M-/] 'complete-dabbrev)</code>
</p>

<p>To bind "M-<tab>" to <code>complete-elisp</code>
in <code>emacs-lisp-mode</code>, you would bind the command in
the <code>emacs-lisp-mode-map</code> keymap:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>(define-key emacs-lisp-mode-map [?\M-\t] 'complete-elisp)</code>
</p>

<p>You're free to bind the <code>complete-&lt;name&gt;</code> commands to
any keys of your choosing, though "M-<tab>" or "M-/" fit best with the
default Completion-UI key bindings that are enabled when you're
completing a word. These are:</p>

<dl>
  <dt>M-&lt;tab&gt;  M-/</dt>
  <dd>Cycle through completions.</dd>

  <dt>M-S-&lt;tab&gt;  M-?</dt>
  <dd>Cycle backwards through completions.</dd>

  <dt>C-&lt;ret&gt;</dt>
  <dd>Accept the current completion.</dd>

  <dt>C-&lt;del&gt;</dt>
  <dd>Reject the current completion.</dd>

  <dt>&lt;tab&gt;</dt>
  <dd>Traditional tab-completion, i.e. insert longest common substring.</dd>

  <dt>C-&lt;tab&gt;</dt>
  <dd>Accept current completion and re-complete the resulting word.</dd>

  <dt>S-&lt;down&gt;</dt>
  <dd>Display the completion tooltip (then use &lt;up&gt; and
  &lt;down&gt; to cycle).</dd>

  <dt>M-&lt;down&gt;</dt>
  <dd>Display the completion menu.</dd>

  <dt>C-&lt;down&gt;</dt>
  <dd>Display the completion pop-up frame.</dd>

  <dt>S-&lt;up&gt; C-&lt;up&gt; M-&lt;up&gt; (in pop-up frame)</dt>
  <dd>Dismiss the completion pop-up frame.</dd>

  <dt>M-/ (in pop-up frame)</dt>
  <dd>Toggle between displaying all completions.</dd>
</dl>



<h3 id="completion-docs">Documentation</h3>

<p>If you're an Emacs user interested in using Completion-UI (as opposed
to an Elisp programmer interesting in extending Completion-UI or making
use of it in your own package), then the first section of
the <a href="#completion-ui-manual">Completion-UI manual</a> would
contain all the information you could possible need...were it to
exist. In the meantime, the description given above should get you
started, and all the commands and customization options are reasonably
well documented via the standard, built-in, Emacs documentation
features. All the relevant customization options can be found in
the <code>completion-ui</code> customization group (and subgroups
thereof).


<p>If you're interested in extending Completion-UI with a new
user-interface or new completion source, or want to make use of
Completion-UI in your own package, then full documentation will shortly
be available in a variety of formats, for download and viewing online. In
the meantime, the docstrings
for <code>completion-ui-register-interface</code>
and <code>completion-ui-register-source</code> should get you
started.</p>


<h4 id="completion-ui-manual">Completion-UI Manual</h4>
<img src="images/men_at_work.png">

<!--
<ul class="files">
  <li>
    <span class="files"><a
    href="predictive/completion-ui-manual/html/index.php"><img
    src="images/icons/link.gif"> Web documentation</a></span>
  </li>
  <li>
    <span class="files"><a
    href="download.php?file=predictive/completion-ui-manual/completion-ui-manual-html.tar.gz"><img
    src="images/icons/html.gif"> HTML download (gzipped tar
    file)</a></span>
  <li>
    <span class="files"><a href="download.php?file=predictive/completion-ui-manual/completion-ui-manual.info.gz"><img
    src="images/icons/info.gif"> Info document (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/completion-ui-manual/completion-ui-manual.txt.gz"><img
    src="images/icons/txt.gif"> ASCII text (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/completion-ui-manual/completion-ui-manual.pdf"><img
    src="images/icons/pdf.gif"> PDF file</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/completion-ui-manual/completion-ui-manual.ps.gz"><img
    src="images/icons/ps.gif"> Postscript file (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/completion-ui-manual/completion-ui-manual.texinfo"><img
    src="images/icons/texi.gif"> Texinfo source </a></span>
  </li>
</ul>
-->



<h3 id="completion-download">Download and Installation</h3>

<p>The current release of the Completion-UI package is version 0.11.11 (released January 2012).
However, it's still under active development at the moment, so don't
forget to check back here for updates every so often. If you want to live
on the bleeding edge, the latest "development" version of the
Completion-UI package is hosted in the same git repository as the
Predictive Completion package.
<!-- (A few older versions are also available below, but are no longer
supported.) -->
</p>

<p>
<ul>
  <li>
    <span class="files">
      Git repository: <code>http://www.dr-qubit.org/git/predictive.git</code>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/completion-ui-0.11.11.tar.gz">      <img src="images/icons/tar.gif"> Current version (0.11.11)</a>
    </span>
  </li>
<!--
  <li>
    <span class="files">
      <a href="download.php?file=predictive/completion-ui-0.8.2.el">      <img src="images/icons/elisp.gif"> Version 0.8.2</a>
    </span>
  </li>
-->
</ul>
</p>

<p>Note that the git repository URL is a git repository, <em>not</em> a
web-site. You <em>cannot</em> view it in a web browser. To grab the
latest development version, clone the repository using something
like:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>git clone http://www.dr-qubit.org/git/predictive.git</code></p>

<p>To install, simply extract the files to a directory in your Emacs
<code>load-path</code>, and add the line:</p>

<p>&nbsp;&nbsp;&nbsp;<code>(require 'completion-ui)</code></p>

<p>to your <code>.emacs</code> file.</p>





<h2 id="overlay">Automatic Overlays</h2>

<p>The Automatic Overlays package allows you to define overlays that
are created (and updated and destroyed) automatically when text in a
buffer matches a regular expression.</p>

<p>Various classes of automatic overlay are provided, to make it easy
to define matches for different text regions: words, lines, regions
enclosed by start and end tags, or regions enclosed by delimiters. You
can also define your own custom classes.</p>

<p>The overlays are updated just before any buffer modification. The
built in overlay classes only update as much as is necessary to ensure
that overlays covering the point are consistent. Therefore the
overlays at the point are guaranteed to be correct before any buffer
modification takes place there, but updating the overlays takes very
little time and causes no noticeable delay.</p>


<h3 id="overlay-docs">Documentation</h3>
<p>Full documentation is available in a variety of formats, for
download or for viewing online. Since Auto-Overlays is an Elisp
library, intended to be used by other Elisp packages, the
documentation is aimed at Elisp programmers, not Emacs users.</p>

<h4 id="auto-overlay-manual">Auto-Overlay Manual</h3>
<ul class="files">
  <li>
    <span class="files"><a
    href="predictive/auto-overlay-manual/html/index.php"><img
    src="images/icons/link.gif"> Web documentation</a></span>
  </li>
  <li>
    <span class="files"><a
    href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual-html.tar.gz"><img
    src="images/icons/html.gif"> HTML download (gzipped tar
    file)</a></span>
  <li>
    <span class="files"><a href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual.info.gz"><img
    src="images/icons/info.gif"> Info document (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual.txt.gz"><img
    src="images/icons/txt.gif"> ASCII text (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual.pdf"><img
    src="images/icons/pdf.gif"> PDF file</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual.ps.gz"><img
    src="images/icons/ps.gif"> Postscript file (gzipped)</a></span>
  </li>
  <li>
    <span class="files"><a href="download.php?file=predictive/auto-overlay-manual/auto-overlay-manual-src.tar.gz"><img
    src="images/icons/texi.gif"> Texinfo source (gzipped tar
    file)</a></span>
  </li>
</ul>



<h3 id="overlay-download">Download and Installation</h3>

<p>The current release of the Automatic Overlays package is version
0.10.7 (released January 2012). However, it's still under active development at
the moment, so don't forget to check back here for updates every so
often. If you want to live on the bleeding edge, the latest "development"
version of the Automatic Overlays package is hosted in the same git
repository as the Predictive Completion package.
<!-- (A few old version are available below, but are no longer
maintainted.) -->
</p>

<p>
<ul>
  <li>
    <span class="files">
      Git repository: <code>http://www.dr-qubit.org/git/predictive.git</code>
    </span>
  </li>
  <li>
    <span class="files">
      <a href="download.php?file=predictive/auto-overlays-0.10.7.tar.gz">      <img src="images/icons/tar.gif"> Current version (0.10.7)</a>
    </span>
  </li>
<!--
  <li>
    <span class="files">
      <a href="download.php?file=predictive/auto-overlays-0.10.5.tar.gz">      <img src="images/icons/tar.gif"> Version 0.10.5</a>
    </span>
  </li>
-->
</ul>
</p>

<p>Note that the git repository URL is a git repository, <em>not</em> a
web-site. You <em>cannot</em> view it in a web browser. To grab the
latest development version, clone the repository using something
like:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>git clone http://www.dr-qubit.org/git/predictive.git</code></p>

<p>To install, simply extract the files to a directory in your Emacs
<code>load-path</code>.</p>




<h2 id="structures">Data Structures</h2>

<p>These packages provide basic (and not so basic) data
structures.</p>


<h3 id="structures-docs">Documentation</h3>

<p>The functions these packages provide are well documented using Emacs'
built-in documentation features. Brief descriptions of the data
structures follow:</p>

<h4>Heaps</h4>
<p>A heap is a form of efficient self-sorting tree. In particular, the
root node is guaranteed to be the highest-ranked entry in the tree. (The
comparison function used for ranking the data can, of course, be freely
defined). They are often used as priority queues, for scheduling tasks in
order of importance, and for implementing efficient sorting algorithms
(such as heap-sort).</p>

<h4>Queues</h4>
<p>A queue can be used both as a first-in last-out and as a first-in
first-out stack, i.e. elements can be added to and removed from the front
or back of the queue. (This library is an updated re-implementation of
the old Elib queue library.)</p>

<h4>Tagged Non-deterministic Finite state Automata</h4>
<div class="annotate"><p>*Regexps are <em>not</em> regular expressions!
Features of modern regexp implementations, including Emacs', mean they
can recognise much more than regular languages. This comes with a big
downside: matching regexps can be very inefficient.</p></div>

<p>A tagged,
<a href="http://en.wikipedia.org/wiki/Nondeterministic_finite_state_machine">non-deterministic
finite state automata</a> (NFA) is an abstract computing machine that
recognises <a href="http://en.wikipedia.org/wiki/Regular_language">regular
languages</a>. In layman's terms, they are used to decide whether a
string matches a regular expression*. The "tagged" part lets the NFA do
group-capture: it returns information about which parts of a string
matched which subgroup of the regular expression. Why re-implement
regular expression matching when Emacs comes with extensive built-in
support for regexps? Primarily, because some algorithms require access to
the NFA states produced part way through the regular expression matching
process. Secondarily, because Emacs regexps only work on strings, whereas
regular expressions can equally well be used to specify other sequence
types.</p>

<h4>Tries</h4>
<div class="annotate"><p>The ternary search tree package has been
obsoleted by the trie package. The former is no longer
supported.</p></div>

<p>A trie stores data associated with "strings" (not necessarily the
string data type; any ordered sequence of elements can be used). It
stores them in such a way that both storage size and data lookup are
reasonably space- and time- efficient, respectively. But, more
importantly, advanced string queries are also very efficient, such as
finding all strings with a given prefix, or finding all strings matching
a regular expression, returning results in alphabetical order, or any
other sort-order, or returning only the first few results, etc.</p>

<h4>Dictionary trees</h4>
<p>The dictionary tree data structures are a hybrid between tries and
hash tables. Data is stored in a trie, but results that take particularly
long to retrieve are cached in hash tables, which are automatically
synchronised with the trie. The dictionary package provides persistent
storage of the data structures in files, and many other convenience
features.<p>


<h3 id="structures-download">Download and Installation</h3>

<p>These packages are all relatively stable, though bug-fixes and new
features are added occasionally. (Latest update: May 2012).</p>

<p>In recent versions of Emacs (&ge;24.1), you can also install all the
non-obsolete packages from within Emacs itself,
via <a href="http://elpa.gnu.org/">GNU ELPA</a>. Use <code>M-x
list-packages</code> and take it from there. (The ELPA version might
occasionally lag slightly behind the latest version available here.)</p>

<p>If you want to see if there are any bleeding-edge changes, the latest
"development" versions of the data structure libraries are hosted in the
same git repository as the Predictive Completion package.</p>

<p>
<ul>
  <li>
    <span class="files">
      Git repository: <code>http://www.dr-qubit.org/git/predictive.git</code>
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/heap-0.3.el">      <img src="images/icons/elisp.gif"> heap.el</a>
      (version 0.3)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/queue-0.1.el">      <img src="images/icons/elisp.gif"> queue.el</a>
      (version 0.1)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/avl-tree-0.2.el">      <img src="images/icons/elisp.gif"> avl-tree.el</a>
      (already included in Emacs &ge; 24.1)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/tNFA-0.1.1.el">      <img src="images/icons/elisp.gif"> tNFA.el</a>
      (version 0.1.1; requires <code>queue.el</code>)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/tstree-0.7.4.el">      <img src="images/icons/elisp.gif"> tstree.el</a>
      (obsolete! Use <code>trie.el</code> instead)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/trie-0.2.5.el">      <img src="images/icons/elisp.gif"> trie.el</a>
      (version 0.2.5; requires everything except <code>dict-tree.el</code>)
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=predictive/dict-tree-0.12.7.el">      <img src="images/icons/elisp.gif"> dict-tree.el</a>
      (version 0.12.7; requires everything else)
    </span>
  </li>
</ul>
</p>

<p>Note that the git repository URL is a git repository, <em>not</em> a
web-site. You <em>cannot</em> view it in a web browser. To grab the
latest development version, clone the repository using something
like:</p>

<p>&nbsp;&nbsp;&nbsp;&nbsp;
<code>git clone http://www.dr-qubit.org/git/predictive.git</code></p>

<p>To install, simply copy the files to a directory in your
Emacs <code>load-path</code>.</p>

<div class="alert"><p>Note that the <code>avl-tree.el</code> library is
an enhanced version of the library of the same name that comes with
Emacs, and is a drop-in replacement for that original version. If you
don't want to overwrite that version, make sure you save this replacement
version to a location that appears <emph>earlier</emph> in your
Emacs <code>load-path</code> than the directory in which the original
version is located (usually something
like <code>/usr/share/emacs/lisp/emacs-lisp/</code> on *nix systems).</p>

<p>You can view the current load path using <code>C-h v load-path</code>
from within Emacs, and check that the new version shadows the original
one by looking at the output of <code>M-x
list-load-path-shadows</code>.</p>

<p>Recent versions of Emacs (&ge;24.1) already include this updated
version of <code>avl-tree.el</code>.</div>




<h2 id="misc">Miscelaneous</h2>

<p>These packages provide miscelaneous features I needed at some point.
So I coded them. Currently, they're all to do with displaying useful
information in the mode line.</p>

<p><code>show-point-mode</code> displays the current value of the point
in the mode line. I primarily find it useful when debugging Elisp code
that uses overlays and markers.</p>

<p><code>wc-mode</code> displays output similar to the
Unix <code>wc</code> command in the mode line, i.e. the character count,
word count and line count for the current buffer. (Useful when writing
grant applications with character or word limits! Though I'm sure it's
useful for other more useful activities, too...)</p>

<p>
<ul>
  <li>
    <span class="files">
      <a href="download.php?file=emacs-misc/show-point-mode-0.2.el">      <img src="images/icons/elisp.gif">
      show-point-mode.el (version 0.2)</a>
    </span>
  </li>

  <li>
    <span class="files">
      <a href="download.php?file=emacs-misc/wc-mode-0.1.el">      <img src="images/icons/elisp.gif">
      wc-mode.el (version 0.1)</a>
    </span>
  </li>
</ul>
</p>



<div id="copyright">&copy;2005-2012 Toby Cubitt</div>
</div>  <!-- main -->
</div>  <!-- page0 -->
</div>  <!-- page1 -->
</div>  <!-- page2 -->
</div>  <!-- content -->


<div class="footer"></div>


<div id="navbar">
<div class="keys">
  <div class="keysL"></div>
  <div class="keys0">
    <a href="computing.html">
      <img src="keys/keys0_down.jpg" alt="[*]">
      <span>Back</span>
    </a>
  </div>
  <div class="keys1">
    <a href="matlab.php">
      <img src="keys/keys1_down.jpg" alt="[*]">
      <span>Maths</span>
    </a>
  </div>
  <div class="keys2">
    <a href="emacs.php">
      <img src="keys/keys2_down.jpg" alt="[*]">
      <span>Emacs</span>
    </a>
  </div>
  <div class="keys3">
    <a href="latex.php">
      <img src="keys/keys3_down.jpg" alt="[*]">
      <span>LaTeX</span>
    </a>
  </div>
  <div class="keys4"></div>
  <div class="keysR"></div>
</div>  <!-- keys -->
</div>  <!-- navbar -->


</body>
</html>
