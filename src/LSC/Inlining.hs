
module LSC.Inlining where


import LSC.Types


inline :: Netlist -> Netlist
inline = inlineAll


inlineAll :: Netlist -> Netlist
inlineAll (Netlist name pins subs nodes edges) = Netlist name

  pins
  subs
  nodes
  edges


