import { Light as SyntaxHighlighter } from "react-syntax-highlighter";

const purescript = (hljs) => {
  var COMMENT = {
    variants: [
      hljs.COMMENT("--", "$"),
      hljs.COMMENT("{-", "-}", {
        contains: ["self"],
      }),
    ],
  };

  var ESCAPED_KEY = {
    className: "attribute",
    begin: '"',
    illegal: '"',
    end: '"',
    relevance: 0,
  };

  // var KEY = {
  //   className: "attribute",
  //   begin: "[a-z]",
  //   illegal: ""
  //   end: "(::|∷)",
  //   excludeEnd: true,
  //   relevance: 0,
  // };

  var CONSTRUCTOR = {
    className: "type",
    begin: "\\b[A-Z][\\w']*", // TODO: other constructors (build-in, infix).
    relevance: 0,
  };

  var LIST = {
    begin: "\\(",
    end: "\\)",
    illegal: '"',
    contains: [
      { className: "type", begin: "\\b[A-Z][\\w]*(\\((\\.\\.|,|\\w+)\\))?" },
      hljs.inherit(hljs.TITLE_MODE, { begin: "[_a-z][\\w']*" }),
      COMMENT,
    ],
  };

  var VALUE = {};

  var KEY_VALUE = {
    className: "attribute",
    begin: "\\w+",
    end: ",|}",
    contains: [VALUE],
  };

  var RECORD = {
    begin: "{",
    end: "}",
    contains: [KEY_VALUE],
  };

  return {
    name: "PureScript",
    aliases: ["purs", "purescript"],
    keywords: {
      keyword:
        "case " +
        "class " +
        "data " +
        "derive " +
        "ado " +
        "do " +
        "else " +
        "if " +
        "import " +
        "in " +
        "infix " +
        "infixl " +
        "infixr " +
        "instance " +
        "let " +
        "module " +
        "newtype " +
        "of " +
        "then " +
        "type " +
        "where " +
        ": " +
        "foreign " +
        "forall " +
        "∀",
    },
    contains: [
      // Top-level constructions.

      {
        beginKeywords: "module",
        end: "where",
        keywords: "module where",
        contains: [LIST, COMMENT],
        illegal: "\\W\\.|;",
      },
      {
        begin: "\\bimport\\b",
        end: "$",
        keywords: "import as hiding",
        contains: [LIST, COMMENT],
        illegal: "\\W\\.|;",
      },

      {
        className: "class",
        begin: "^(\\s*)?(class|instance)\\b",
        end: "where",
        keywords: "class instance where",
        contains: [CONSTRUCTOR, LIST, COMMENT],
      },
      {
        className: "class",
        begin: "\\b(data|(new)?type)\\b",
        end: "$",
        keywords: "data family type newtype derive",
        contains: [CONSTRUCTOR, LIST, RECORD, COMMENT],
      },
      {
        className: "symbol",
        begin: "=|<-|->|\\:\\:|\\\\|=>|<=|forall|∀|\\:|∷|{|}|\\(|\\)|\\[|\\]|,",
        invalid: "/",
      },
      {
        beginKeywords: "default",
        end: "$",
        contains: [CONSTRUCTOR, LIST, COMMENT],
      },
      {
        beginKeywords: "infix infixl infixr",
        end: "$",
        contains: [hljs.C_NUMBER_MODE, COMMENT],
      },
      {
        begin: "\\bforeign\\b",
        end: "$",
        keywords: "foreign import",
        contains: [CONSTRUCTOR, hljs.QUOTE_STRING_MODE, COMMENT],
      },

      // "Whitespaces".

      // Literals and names.

      // TODO: characters.
      hljs.QUOTE_STRING_MODE,
      hljs.C_NUMBER_MODE,
      CONSTRUCTOR,
      hljs.inherit(hljs.TITLE_MODE, { begin: "^[_a-z][\\w']*" }),

      COMMENT,

      { begin: "->|<-" }, // No markup, relevance booster
    ],
  };
};

SyntaxHighlighter.registerLanguage("puresscript", purescript);
exports.syntaxHighlighterImpl = SyntaxHighlighter;
