IMAGE_NAME = ft_turing
PROJECT_DIR = $(shell pwd)/project
CONTAINER_NAME = turing

all: build shell

build:
	docker build -t $(IMAGE_NAME) .

shell:
	docker run --name $(CONTAINER_NAME) -it --rm $(IMAGE_NAME) bash

clean:
	docker stop $(CONTAINER_NAME)

fclean: clean
	docker image rm -f $(IMAGE_NAME) 

re: fclean all

.PHONY: build shell clean fclean re
