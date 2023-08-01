[
  {"selector":"node", "css": {
    "border-width": "2px",
    "content": "data(name)",
    "background-color":"white",
    "border-color":"black"
  }},
  {"selector":"node[nodeType = 'FAMILY']","css":{
    "shape":"rectangle"
  }
  },
  {"selector":"node[nodeType = 'GENE']","css":{
    "shape":"round-rectangle"
  }
  },
  {"selector":"node[nodeType = 'COMPARTMENT']","css":{
    "shape":"barrel",
    "border-width":"4px" 
  }
  },
  {"selector":"node[nodeType = 'COMPLEX']","css":{
    "shape":"cut-rectangle"
  }},
  // Every node background color
   {"selector":"node[avgValues = 'NA']", "css": {
       "background-color": "grey"
  }},
  {"selector":"node[avgValues <= 0]", "css": {
       "background-color": "mapData(avgValues, -10, 0, red, white)"
  }},
  
  {"selector":"node[avgValues > 0]", "css": {
       "background-color": "mapData(avgValues,0,10, white, blue)"
  }},

  {"selector": "node:selected", "css": {
       "overlay-opacity": 0.3,
       "overlay-color": "gray"
  }},
  
  
  // Edge Interactions 
  {"selector":"edge","css":{
    "curve-style":"bezier"
    
  }},

  {"selector": "edge[interaction='INHIBITS']", "css": {
    "line-color": "red",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "red"
  }},
  
  {"selector": "edge[interaction='ACTIVATES']", "css": {
    "line-color": "green",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "green"
  }},
  
  {"selector": "edge[interaction='BINDS']", "css": {
    "line-color": "black",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "black"
  }},
  
  {"selector": "edge[interaction='INDUCES']", "css": {
    "line-color": "green",
    "line-style":"dashed",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "green"
  }},
  
  {"selector": "edge[interaction='REPRESSES']", "css": {
    "line-color": "red",
    "line-style": "dashed",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "red"
  }}
]