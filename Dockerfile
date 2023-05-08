FROM alpine as zola
RUN apk add --no-cache zola
WORKDIR /app
COPY . . 
RUN zola build

FROM dshadow/static-http-server as blog
COPY --from=zola /app/public/ /www
