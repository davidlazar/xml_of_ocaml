# About

This program and library pretty-prints the abstract syntax tree (AST) of an OCaml program as XML.

# Example

    $ cat let.ml
    let x = 7 in x + 3;;

    $ xml_of_ocaml let.ml | <xml_pretty_printer>
    <StExp>
      <ExLet>
        <BiEq>
          <PaId>
            <IdLid>
              <Builtin><String>x</String></Builtin>
            </IdLid>
          </PaId>
          <ExInt>
            <Builtin><Integer>7</Integer></Builtin>
          </ExInt>
        </BiEq>
        <ExApp>
          <ExApp>
            <ExId>
              <IdLid>
                <Builtin><String>+</String></Builtin>
              </IdLid>
            </ExId>
            <ExId>
              <IdLid>
                <Builtin><String>x</String></Builtin>
              </IdLid>
            </ExId>
          </ExApp>
          <ExInt>
            <Builtin><Integer>3</Integer></Builtin>
          </ExInt>
        </ExApp>
      </ExLet>
    </StExp>

# Contributing

This project is available on [GitHub](https://github.com/davidlazar/xml_of_ocaml) and [Bitbucket](https://bitbucket.org/davidlazar/xml_of_ocaml/). You may contribute changes using either.

Please report bugs and feature requests using the [GitHub issue tracker](https://github.com/davidlazar/xml_of_ocaml/issues).
