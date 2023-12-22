+++
title = "About"
disable_feed = true
toc = false
lastMod = "2023-07-21"
+++

Website source is available [on GitHub](https://github.com/SandaruKasa/blog).

Stuff used:
- [Hugo](https://gohugo.io/): converts `.md` files to `.html` for the blog.
- [Anubis](https://github.com/Mitrichius/hugo-theme-anubis): HTML templates for hugo.
- A touch of [custom CSS](/css/scratch.css), which I stole from some smart ballsy guy.
- [Gitea](https://gitea.io/): the GitLab at home. Fun fact: it uses Hugo too.
- [Caddy](https://caddyserver.com/): subdomain routing, HTTP reverse-proxiyng, serving static files, ~~SSL~~ TLS magic.
- [Docker](https://www.docker.com/) + docker-compose: orchestrating everything.

Funnily enough, all of the software mentioned above is written in Go.

Additionally, I used to use [FastHttpd](https://fasthttpd.org/) (which is also written in Go)
to serve the blog, and Caddy would just reverse-proxy to it,
but then I figured Caddy can serve static files too, so I got rid of FastHttpd.

Anyways, I go into more detail about this site in [the first blogpost]({{< ref "/posts/2023-05-09-hello-world">}}).
