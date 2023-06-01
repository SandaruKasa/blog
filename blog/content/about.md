+++
title = "About"
path = "about"
+++

Website source is available [on GitHub](https://github.com/SandaruKasa/sklad.observer).

Stuff used:
- [Zola](https://www.getzola.org/): converts `.md` files into `.html` for the blog.
- [apollo](https://www.getzola.org/themes/apollo/), a Zola theme, +
a touch of custom css ~~to make the site look awful~~.
- [Gitea](https://gitea.io/) for git.
- [caddy](https://caddyserver.com/): subdomain routing + https magic.
- [Docker](https://www.docker.com/) + docker-compose: put everything together.

Funnily enough, almost all of the above is written in Go.
With the exception of Zola (Rust) and Apollo (basically a bunch of jinja, html, & css).

Additionally, I used to use [FastHttpd](https://fasthttpd.org/) to serve the blog,
and Caddy would just reverse-proxy to it, but then I figured Caddy can serve static files too,
so I got rid of FastHttpd. It was also written in Go though.

Anyways, I go into more detail about this site in [the first blogpost](/posts/hello-world).
