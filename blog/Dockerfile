FROM alpine as zola
RUN apk add --no-cache zola
WORKDIR /run
COPY . .
# Zola is a bitch, and tries to delete the output directory even if it's empty.
CMD mkdir -p public && rm -rf tmp/ public/* && zola build -o tmp && mv tmp/* public/ && rmdir tmp
