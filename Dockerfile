FROM ocaml/opam:debian-ocaml-5.1

USER root
RUN apt-get update && apt-get install -y m4 pkg-config libgmp-dev libffi-dev git make

USER opam
WORKDIR /app

COPY --chown=opam ./project ./project

RUN opam install -y $(cat ./project/.project-deps | tr '\n' ' ')

WORKDIR /app/project

CMD ["bash"]
