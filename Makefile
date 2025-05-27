IMAGE_NAME = ft_turing
PROJECT_DIR = $(shell pwd)/project

all: build shell

build:
	docker build -t $(IMAGE_NAME) .

shell:
	docker run -it --rm -v $(PROJECT_DIR):/home/opam/project -w /home/opam/project $(IMAGE_NAME) bash

clean:
	docker run --rm -v $(PROJECT_DIR):/home/opam/project -w /home/opam/project $(IMAGE_NAME) make clean

fclean:
	docker run --rm -v $(PROJECT_DIR):/home/opam/project -w /home/opam/project $(IMAGE_NAME) make fclean
	docker image rm -f $(IMAGE_NAME) 

re: fclean all
