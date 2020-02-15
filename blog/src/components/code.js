import React, { useEffect, useState } from "react";
const yogaCard = require("./YogaCard.purs");
const fetch = typeof window !== "undefined" && window.fetch;

const Code = ({ codeString, language, ...props }) => {
  const [Editor, setEditor] = useState(null);
  console.log("stuff", props);
  useEffect(() => {
    if (Editor === null) {
      const Eddy = yogaCard.mkEditor(fetch)();
      setEditor(prev => console.log("Prev", prev) || Eddy);
    }
  }, []);

  return (
      Editor !== null ? <Editor height="300px" initialCode={codeString}></Editor> : ""
  );
};

export default Code;
