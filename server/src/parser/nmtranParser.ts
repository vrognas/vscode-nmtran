const nearley = require("nearley");
const grammar = require("./nmtranGrammar.js");

const nmtranParser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

try {
  nmtranParser.feed("a=1222.5");
  
  console.log("Parse successful.", nmtranParser.results);
} catch (e: any) {
  console.log(`Parse error: ${e.message}`);
}
