About Reader
------------
Reader is a simple blogging platform for [Radiance](https://shinmera.com/project/Radiance).

How To
------
Set up Radiance and load Reader through Quicklisp or ASDF. Reader occupies the subdomain `reader`. You can start writing blogs on `reader./write`. Editing blogs happens on the same page, by appending `/ID` with ID being the blog-entry ID.

Install
-------
0. Make sure the [Shirakumo dist](https://shirakumo.org/project/dist) is installed and [Radiance](https://shinmera.com/project/Radiance) is set up.
1. Load reader: `(ql:quickload :reader)`
2. Set the title: `(setf (radiance:mconfig :reader :title) "My Fancy Blog")`
3. Start radiance: `(radiance:startup)`
4. Visit <http://radiance.test:8080/!/reader/>

Creating and Editing Entries
----------------
0. Make sure you have a user `(user:get "username" :if-does-not-exist :create)`
1. Grant it access rights `(user:grant "username" '(reader write))`
2. Log in
3. Visit <http://radiance.test:8080/!/reader/write/>
4. To edit, simply change `/article/` in the URL of an entry to `/write/`.

Interface Dependencies
----------------------
* database
* data-model
* auth
* cache
* profile

Configuration Variables
-----------------------
* `(:title)`
* `(:description)`
* `(:markup)` The blog markup to use -- either `:markdown`, `:markless`, `:html`, or `:plain`. Defaults to `:markdown`.

Permissions
-----------
* `(reader write)`
