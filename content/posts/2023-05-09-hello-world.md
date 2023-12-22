+++
title = "Hello, world!"
slug = "hello-world"
summary = "This site now exists."
tags = ["meta", "web"]

date = "2023-05-09"
lastMod = "2023-07-21"
aliases = ["/posts/2023-05-09-hello-world"]
+++

## Getting the domain name

### But which one do I even want?

There's this [cool site with a lot of info on TLDs and whatnot](https://tld-list.com/tlds-from-a-z).
An interesting thing to scroll through.

Did you know there's a `.gay` TLD? How about `.lgbt`, `.travelersinsurance`,
`.microsoft`, or `.google`?
Maybe you would be interested in some `.ком`, which is just `.com` but in Cyrillic,
because I guess `.com.ru` wasn't an option?

But anyways, there are way more other strange TLDs out there, buy you can take a look yourself;
this post is about *this* site, and for this site I initially thought of `sklad.sucks`,
but apparently `.sucks` kinda sucks: it costs quite a sum & the fact that `get.sucks`
(registrar's website) was down when I was making my choice didn't really help.

Eventually I settled on... well, you already know.
`.observer` sounded cool and even turned out to be one of the cheapest option out there.

### Buying the thing

[Spaceship](https://www.spaceship.com/). Amazing registrar, can't name a single flaw:
+ had the lowest price out there,
+ helps Ukraine out a ton by not letting you register a TLD if you're in Russia,
+ actually good website UI, doesn't make you want to tear your eyeballs out
& slam the designer's head against a wall (which is, sadly, a rarity nowadays);
+ 🚀blazingly fast✨ domain propagation. Seriously, the DNS records I set
take less than a minute to get from the control panel to `8.8.8.8` & `1.1.1.1`.
+ They also didn't try to sell me things that I don't need
and charge extra with some misleading price tags.
Which is, unfortunately, also a bit of a rare thing nowadays.
Especially with `reg.ru`. Oh god, `reg.ru`.
Trying to buy something from them is like trying to disable all the cookies
on an especially annoying website.
Not to mention their prices and the fact that you have to pay extra for WHOIS protection.
Funnily enough, other Russian registrars charge even more, so if you compare
Russian prices in dollars to USA prices, it turns out that $1 = 150₽,
which is two times more than what Google or some bank gives you (77₽ as of writing this).

## Making the website

### Blog

Static HTML. Rendered from `.md` files with the help of [Zola](https://www.getzola.org/).

Not much to say here.

The theme I've used is [apollo](https://www.getzola.org/themes/apollo/).
And no, it is not responsible for the awful color palette you're seeing.

The static content is than served with [FastHttpd](https://fasthttpd.org/) & proxied through Caddy.
But I guess I can just move the static files into the Caddy container.

Speaking of which,

### Caddy

It does miracles.

[Caddy](https://caddyserver.com/) is basically an http proxy/server,
but damn is it versatile.

Subdomain routing? Piece of cake. Just
```caddy
sklad.observer, www.sklad.observer {
	redir https://blog.sklad.observer{uri}
	log
}
```

A reverse proxy?
```caddy
gitea.sklad.observer {
	reverse_proxy http://SOME_ADDRESS:80
}
```

Everything is https by default, out of the box, zero effort required.
Any domain that you mention automatically gets considered `HTTPS`
(unless you explicitly prefix it with `http://`),
and than Caddy takes care of the rest:
+ It goes out to Let's Encrypt and automatically requests certificates.
+ It goes through the entire procedure of verifying the ownership of the domain
with no extra input required from you
(just make sure the DNS for the domain is pointing at wherever Caddy is running).
+ It automatically sets up the redirects from HTTP on `:80` to HTTPS on `:443`.
+ And, when the time comes, it renews the certificates too.

The logging capabilities it provides are awesome too.
The logs are well structured, have many output options, the defaults settings are really good,
and the logs themselves include all the important info you might need to troubleshoot a problem.

The format for settings, the "`Caddyfile`", is nicely readable and pleasant to write,
albeit takes a tiniest amount of time to get used to.
But if it's not your cup of tea or if the config is written by a machine and not a human,
you can just use `JSON`, Caddy supports that too.

### Docker

With compose.
Probably the easiest way to set up some THING consisting of heterogenous compound parts.

Portable, reproducible, convenient. What else can you ask for?
(Well, maybe a better format than `YAML` for config files, but whatever,
I'm not in Norway, so not much trouble aside from port bindings
being treated as hexadecimal numbers.)

### Git

[Gitea](https://gitea.io/) was my choice.
Minimalistic, easy to set up, pleasant to use.
Not much to say here, because it never gave me any trouble (so far).

It's written in Go. Which bring me to my next topic.

## Closing thoughts

### Golang

A really good async runtime, coupled with a... *peculiar* language to say the least.
But does that really matter if this language powers so much good stuff.
Like this website (not to imply it's an exemplar of a "good stuff", but):
* Docker
* Caddy
* Gitea
* FastHttpd

all written in go.
Heh.

Time to post about Rust I guess.

### The source code

Is it really "code"?
It's mostly just configs and assets, but whatever.

+ [GitHub](https://github.com/SandaruKasa/blog)
+ [Gitea](https://gitea.sklad.observer/SandaruKasa/blog)

## UPD
I've got rid of FastHttpd, because Caddy can serve static files too. \
That's one less (fewer?) Golang dependency.

I've switched from Zola to [Hugo](https://gohugo.io/), which is written in Go. \
That's one more Golang dependency.

Zola was also the only non-Golang dependency, so now this site is 100% Go, heh.

But yeah, Zola just felt like an unfinished & shabby rewrite of Hugo into Rust,
and wasn't pleasant to deal with overall.
Thanks, Luke Smith, for [introducing me to Hugo](https://youtu.be/jAXKSKb3etk).
