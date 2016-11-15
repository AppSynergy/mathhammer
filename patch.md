````
case 'p-text':
  if (typeof domNode !== 'undefined') {
    domNode.replaceData(0, domNode.length, patch.data);
  }
  return domNode;
````

elm-stuff/packages/elm-lang/virtual-dom/1.1.1/src/Native/VirtualDom.js1298
