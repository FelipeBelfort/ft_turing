NAME = ft_turing
SRC = main.ml
BUILD = build

OCAMLOPT = ocamlfind ocamlopt
PKGS := $(shell grep -v '^ocamlfind$$' .project-deps | tr '\n' ' ')

all: $(BUILD) $(NAME)

$(BUILD):
	mkdir -p $(BUILD)

$(NAME): $(SRC)
	$(OCAMLOPT) -package $(PKGS) -linkpkg -I $(BUILD) -o $(NAME) $(SRC)
	mv *.cm* *.o $(BUILD) 

clean:
	rm -rf $(BUILD)

fclean: clean
	rm -f $(NAME)

re: fclean all
