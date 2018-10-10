FROM haskell:8.2.2 as builder

WORKDIR /app
ADD . /app

RUN stack setup
RUN stack build --copy-bins

FROM fpco/haskell-scratch:integer-gmp
WORKDIR /root/
COPY --from=builder /root/.local/bin/psst-exe .

EXPOSE 3000

CMD ["./psst-exe"]
