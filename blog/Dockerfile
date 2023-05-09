FROM alpine as zola
RUN apk add --no-cache zola
WORKDIR /app
COPY . . 
RUN zola build

FROM fasthttpd/fasthttpd as blog
COPY --from=zola /app/public/ /usr/share/fasthttpd/html/
