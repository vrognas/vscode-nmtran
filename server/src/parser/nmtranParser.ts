import * as nearley from "nearley";
const grammar = require("./nmtranGrammar.js");
import * as fs from "mz/fs";
import * as path from "path";

async function main() {
  const nmtranParser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));

  const filename = process.argv[2];
  const outputDir = "./test/parser";
  if (!fs.existsSync(outputDir)){
    fs.mkdirSync(outputDir, { recursive: true });
  }
  const outputfile = path.join(outputDir, path.basename(filename, ".mod") + ".ast");

  const code = (await fs.readFile(filename)).toString();
  
  try {
    nmtranParser.feed(code);
    const ast = nmtranParser.results[0];
    await fs.writeFile(outputfile, JSON.stringify(ast, null, 2));
    console.log("Parse successful.", nmtranParser.results[0]);
    console.log(`AST written to ${outputfile}`);
  } catch (e: any) {
    console.log(`Parse error: ${e.message}`);
  }
}

main();
