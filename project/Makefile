NAME = ft_turing
BUILD = build

SRC = errors.ml main.ml

OBJ = $(patsubst %.ml,$(BUILD)/%.cmx,$(SRC))

OCAMLOPT = ocamlfind ocamlopt
PKGS := $(shell grep -v '^ocamlfind$$' .project-deps | tr '\n' ' ')

all: $(NAME)

$(BUILD)/%.cmx: %.ml | $(BUILD)
	$(OCAMLOPT) -package $(PKGS) -c -o $@ $<

$(BUILD):
	mkdir -p $(BUILD)

$(NAME): $(BUILD) $(OBJ)
	$(OCAMLOPT) -package $(PKGS) -linkpkg -o $(NAME) $(OBJ)
	# $(OCAMLOPT) -package $(PKGS) -linkpkg -I $(BUILD) -o $(NAME) $(SRC)
	# mv *.cm* *.o $(BUILD) 

clean:
	rm -rf $(BUILD)

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re