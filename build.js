const markdownPDF = require("markdown-pdf");
const fs = require("fs");

fs.createReadStream("src/index.md")
  .pipe(markdownPDF())
  .pipe(fs.createWriteStream("build/review.pdf"))