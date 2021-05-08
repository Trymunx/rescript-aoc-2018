module.exports = {
  root: true,
  env: {
    node: true,
  },
  extends: ["plugin:vue/essential", "@vue/standard", "@vue/typescript"],
  rules: {
    "no-console": process.env.NODE_ENV === "production" ? "error" : "off",
    "no-debugger": process.env.NODE_ENV === "production" ? "error" : "off",
    indent: ["error", 2, { SwitchCase: 1 }],
    "linebreak-style": ["error", "unix"],
    quotes: ["error", "double"],
    semi: ["error", "always"],
    "comma-dangle": ["error", "always-multiline"],
    "max-len": ["error", { code: 100, tabWidth: 2, ignoreTrailingComments: true }],
    "no-trailing-spaces": ["error"],
    "object-curly-spacing": ["warn", "always", { objectsInObjects: false, arraysInObjects: false }],
    "arrow-parens": ["warn", "as-needed"],
    "sort-imports": [
      "warn",
      {
        ignoreCase: true,
        memberSyntaxSortOrder: ["none", "all", "single", "multiple"],
      },
    ],
    "sort-keys": ["warn", "asc", { caseSensitive: true, natural: true }],
    "space-before-function-paren": [
      "error",
      { anonymous: "never", asyncArrow: "never", named: "never" },
    ],
    "generator-star-spacing": ["error", { before: false, after: true }],
    "no-fallthrough": ["error", { commentPattern: "break[\\s\\w]*omitted" }],
  },
  parserOptions: {
    parser: "@typescript-eslint/parser",
  },
};
