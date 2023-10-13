const nearley = require("nearley");
const grammar = require("./nmtran.js");

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

try {
  parser.feed("a=1222.5");
  
  console.log("Parse successful.", parser.results);
} catch (e: any) {
  console.log(`Parse error: ${e.message}`);
}
