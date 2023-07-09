FROM alpine as zola
RUN apk add --no-cache zola
ENTRYPOINT [ "zola" ]
