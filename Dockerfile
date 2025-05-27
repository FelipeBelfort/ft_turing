FROM ocaml/opam:debian-ocaml-5.1

USER root
RUN apt-get update && apt-get install -y m4 pkg-config libgmp-dev libffi-dev git make

USER opam
WORKDIR /home/opam/project

COPY project/.project-deps .project-deps

RUN opam install -y $(cat .project-deps | tr '\n' ' ')

CMD ["bash"]
