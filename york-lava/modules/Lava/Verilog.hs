module Lava.Verilog(writeVerilog) where

{-
BUG:
#1: using both writeVerilog and writeVhdl, or either twice, doesn't work.
    Side-effects corrupting the netlist?

Suggestions for improvements:
- inline everything that isn't shared (drops the # of defined wires dramatically)
- use true Verilog addition rather than try to created it by hand (TBD)
- instead of using individual wires for busses, use the [N-1:0] Verilog notation.
  (mostly affects memories).
-}

import Lava.Bit
import Lava.Binary
import System
import Numeric (showHex)

verilogModuleHeader :: String -> Netlist -> String
verilogModuleHeader name nl =
  "module " ++ name ++ "(\n" ++
  consperse ",\n"
       ([ "  input " ++ v | v <- "clock":inps, check v] ++
        [ "  output " ++ v | v <- outs, check v]) ++
  ");\n\n"
  where
    inps = [ lookupParam (netParams net) "name"
           | net <- nets nl, netName net == "name"]
    outs = map fst (namedOutputs nl)

check name = if name `elem` reserved then
                error $ "`" ++ name ++ "' is a reserved Verilog keyword"
             else
                True
             where
             reserved = ["module", "input", "output", "inout", "reg", "wire", "for",
                         "always", "assign", "begin", "end", "endmodule"]

verilogDecls :: Netlist -> String
verilogDecls nl =
  bracket "  wire " ";\n"
        [ consperse ", " $ map (wireStr . (,) (netId net)) [0..netNumOuts net - 1]
        | net <- wires ]
  ++
  bracket "  reg " ";\n"
        [ consperse ", " $ map (regFormat net) [0..netNumOuts net - 1]
        | net <- regs ]
  where
    bracket pre post xs = concat [pre ++ x ++ post | x <- xs]
    regFormat net 0 = wireStr (netId net, 0) ++ " = " ++ init net
    regFormat net y = error "unexpected output arity of a register"

    -- I'm sure there's a "partition" function for this
    wires = [ net | net <- nets nl
            , netName net /= "delay" && netName net /= "delayEn" ]
    regs = [ net | net <- nets nl
            , netName net == "delay" || netName net == "delayEn" ]
    init :: Net -> String
    init net = lookupParam (netParams net) "init"

type Instantiator = String -> [Parameter] -> InstanceId -> [Wire] -> String

verilogInsts :: Netlist -> String
verilogInsts nl =
  concat [ verilogInst (netName net)
             (netParams net)
             (netId net)
             (netInputs net)
         | net <- nets nl ] ++
  concat [ "  assign " ++ s ++ " = " ++ wireStr w ++ ";\n"
         | (s, w) <- namedOutputs nl ]

verilogInst :: Instantiator
verilogInst "low"     = constant "0"
verilogInst "high"    = constant "1"
verilogInst "inv"     = gate 1 "~"
verilogInst "and2"    = gate 1 "&"
verilogInst "or2"     = gate 1 "|"
verilogInst "xor2"    = gate 1 "^"
verilogInst "eq2"     = gate 1 "=="
verilogInst "xorcy"   = gate 1 "^" -- makes no distinction between xorcy and xor2
verilogInst "muxcy"   = muxcyInst
verilogInst "name"    = assignName
verilogInst "delay"   = delay False
verilogInst "delayEn" = delay True
verilogInst "ram"     = instRam
verilogInst "dualRam" = instRam2
verilogInst "adder"   = instAdder
verilogInst s = error ("Verilog: unknown component '" ++ s ++ "'")

muxcyInst params dst [ci,di,s] =
  "  assign " ++ wireStr (dst, 0) ++ " = " ++
  wireStr s ++ " ? " ++ wireStr ci ++ " : " ++ wireStr di ++ ";\n"

mifFiles :: Netlist -> [(String, String)]
mifFiles nl =
    [ ( "ram_" ++ compStr (netId net) ++ ".mif"
      , genMifContents $ netParams net)
    | net <- nets nl
    , netName net == "ram" || netName net == "dualRam"
    , nonEmpty (netParams net)
    ]
  where
    init params = read (lookupParam params "init") :: [Integer]
    nonEmpty params = not $ null $ init params
    genMifContents params = let dwidth = read (lookupParam params "dwidth") :: Int
                                awidth = read (lookupParam params "awidth") :: Int
      in
       unlines
         [ "-- Generated by York Lava, Californicated"
         , "WIDTH=" ++ show dwidth ++ ";"
         , "DEPTH=" ++ show (2^awidth) ++ ";"
         , "ADDRESS_RADIX=HEX;"
         , "DATA_RADIX=HEX;"
         , "CONTENT BEGIN"
         ]
       ++
       unlines
         [ showHex i (':' : showHex v ";")
         | (i,v) <- zip [0..2^awidth-1] (init params ++ repeat 0)
         ]
       ++
       "END;\n"

verilog :: String -> Netlist -> [(String, String)]
verilog name nl =
  [ (name ++ ".v",

     "// Portable Verilog generated by York Lava, Californicated\n"
     ++ verilogModuleHeader name nl
     ++ verilogDecls nl
     ++ verilogInsts nl
     ++ "endmodule\n") ] ++
  mifFiles nl

{-|
For example, the function

> halfAdd :: Bit -> Bit -> (Bit, Bit)
> halfAdd a b = (sum, carry)
>   where
>     sum   = a <#> b
>     carry = a <&> b

can be converted to a Verilog entity with inputs named @a@ and @b@ and
outputs named @sum@ and @carry@.

> synthesiseHalfAdd :: IO ()
> synthesiseHalfAdd =
>   writeVerilog "HalfAdd"
>             (halfAdd (name "a") (name "b"))
>             (name "sum", name "carry")
-}
writeVerilog ::
  Generic a => String -- ^ The name of VERILOG entity, which is also the
                      -- name of the directory that the output files
                      -- are written to.
            -> a      -- ^ The Bit-structure that is turned into VERILOG.
            -> a      -- ^ Names for the outputs of the circuit.
            -> IO ()
writeVerilog name a b =
  do putStrLn ("Creating directory '" ++ name ++ "/'")
     system ("mkdir -p " ++ name)
     nl <- netlist a b
     mapM_ gen (verilog name nl)
     putStrLn "Done."
  where
    gen (file, content) =
      do putStrLn $ "Writing to '" ++ name ++ "/" ++ file ++ "'"
         writeFile (name ++ "/" ++ file) content

-- Auxiliary functions

compStr :: InstanceId -> String
compStr i = "c" ++ show i

wireStr :: Wire -> String
wireStr (i, 0) = "w" ++ show i
wireStr (i, j) = "w" ++ show i ++ "_" ++ show j

consperse :: String -> [String] -> String
consperse s [] = ""
consperse s [x] = x
consperse s (x:y:ys) = x ++ s ++ consperse s (y:ys)

argList :: [String] -> String
argList = consperse ","

gate 1 str params comp [i1,i2] =
  "  assign " ++ dest ++ " = " ++ x ++ " " ++ str ++ " " ++ y ++ ";\n"
  where dest = wireStr (comp, 0)
        [x,y] = map wireStr [i1,i2]

gate n str params comp [i] =
  "  assign " ++ dest ++ " = " ++ str ++ wireStr i ++ ";\n"
  where dest = wireStr (comp, 0)

gate n str params comp inps = error $ "gate wasn't expecting " ++ str ++ "," ++ show inps

assignName params comp inps =
  "  assign " ++ wireStr (comp, 0)  ++ " = " ++ lookupParam params "name" ++ ";\n"

constant str params comp inps =
  "  assign " ++ wireStr (comp, 0) ++ " = " ++ str ++ ";\n"

v_always_at_posedge_clock stmt = "  always @(posedge clock) " ++ stmt ++ "\n"
v_assign dest source = dest ++ " <= " ++ source ++ ";"
v_if_then cond stmt = "if (" ++ cond ++ ") " ++ stmt
v_block stmts = "begin\n" ++
                concat ["    " ++ s ++ "\n" | s <- stmts ] ++
                "  end\n" -- Indents needs more cleverness, like a Doc

delay :: Bool -> [Parameter] -> Int -> [Wire] -> String
delay False params comp [_, d] =
  v_always_at_posedge_clock (wireStr (comp, 0) `v_assign` wireStr d)

delay True params comp [_, ce, d] =
  v_always_at_posedge_clock (v_if_then (wireStr ce) (wireStr (comp, 0) `v_assign` wireStr d))

vBus :: [Wire] -> String
vBus bus = "{" ++ argList (map wireStr (reverse bus)) ++ "}"

vInst :: String -> Int -> [(String, [Wire])] -> [(String, String)] -> String
vInst name comp args instParams =
    concat ["  ", name, " ", compStr comp, "(", argList (map param args), ");\n"] ++
    "  defparam" ++
    argList [ "\n    " ++ compStr comp ++ "." ++ formal ++ " = " ++ actual
            | (formal, actual) <- instParams ] ++
    ";\n"
  where param (formal, actual) = "." ++ formal ++ "(" ++ vBus actual ++ ")"

{-

Better yet: lpm_add_sub

module lpm_add_sub ( .result(outs), cout, overflow, .add_sub, .cin(c), .dataa(a), .datab(b),
clock, clken, aclr );
parameter lpm_type = "lpm_add_sub";
parameter lpm_width = 1;
parameter lpm_direction  = "UNUSED";
parameter lpm_representation = "UNSIGNED";
parameter lpm_pipeline = 0;
parameter lpm_hint = "UNUSED";
input  [lpm_width-1:0] dataa, datab;
input  add_sub, cin;
input  clock;
input  clken;
input  aclr;
output [lpm_width-1:0] result;
output cout, overflow;
endmodule

  lpm_add_sub cXXX (.result(outs), .cin(c), .dataa(a), .datab(b));
-}

instAdder params comp (c:ab) =
  vInst "lpm_add_sub"
        comp
        [("result", outs), ("cin", [c]), ("dataa", a), ("datab", b)]
        [("lpm_width", show width)]
  where
     outs = map ((,) comp) [0..width-1]
     (a,b) = splitAt width ab
     width = read (lookupParam params "width") :: Int

instAdder' params comp (c:ab) =
   concat ["  assign ", vBus outs, " = ", vBus a, " + ", vBus b, " + ", wireStr c, ";\n" ]
   where
     outs = map ((,) comp) [0..width-1]
     (a,b) = splitAt width ab
     width = read (lookupParam params "width") :: Int

instRam params comp (we1:sigs) =
 let  init = read (lookupParam params "init") :: [Integer]
      initFile = "ram_" ++ compStr comp ++ ".mif"
      dwidth = read (lookupParam params "dwidth") :: Int
      awidth = read (lookupParam params "awidth") :: Int

      (dbus1, abus1) = splitAt dwidth sigs
      outs1          = map ((,) comp) [0..dwidth-1]
      c              = " " ++ compStr comp
 in
  "  altsyncram" ++ c ++ "(\n" ++
  "   .clock0 (clock),\n" ++

  "   .address_a (" ++ vBus abus1 ++ "),\n" ++
  "   .wren_a (" ++ wireStr we1 ++ "),\n" ++
  "   .data_a (" ++ vBus dbus1 ++ "),\n" ++
  "   .q_a (" ++ vBus outs1 ++ "),\n" ++

  "   .address_b (1'b1),\n" ++
  "   .wren_b (1'b0),\n" ++
  "   .data_b (1'b1),\n" ++
  "   .q_b (),\n" ++

  "   .aclr0 (1'b0),\n" ++
  "   .aclr1 (1'b0),\n" ++
  "   .addressstall_a (1'b0),\n" ++
  "   .addressstall_b (1'b0),\n" ++
  "   .byteena_a (1'b1),\n" ++
  "   .byteena_b (1'b1),\n" ++
  "   .clock1 (1'b1),\n" ++
  "   .clocken0 (1'b1),\n" ++
  "   .clocken1 (1'b1),\n" ++
  "   .clocken2 (1'b1),\n" ++
  "   .clocken3 (1'b1),\n" ++
  "   .eccstatus (),\n" ++
  "   .rden_a (1'b1),\n" ++
  "   .rden_b (1'b1));\n" ++
  "  defparam\n" ++
  c ++ ".clock_enable_input_a = \"BYPASS\",\n" ++
  c ++ ".clock_enable_output_a = \"BYPASS\",\n" ++
  (if null init then "" else c ++ ".init_file = \"" ++ initFile ++ "\",\n") ++
  c ++ ".lpm_type = \"altsyncram\",\n" ++
  c ++ ".numwords_a = " ++ show (2^awidth) ++ ",\n" ++
  c ++ ".operation_mode = \"SINGLE_PORT\",\n" ++
  c ++ ".outdata_aclr_a = \"NONE\",\n" ++
  c ++ ".outdata_reg_a = \"UNREGISTERED\",\n" ++
  c ++ ".power_up_uninitialized = \"FALSE\",\n" ++
  c ++ ".read_during_write_mode_port_a = \"NEW_DATA_NO_NBE_READ\",\n" ++
  c ++ ".widthad_a = " ++ show awidth ++ ",\n" ++
  c ++ ".width_a = " ++ show dwidth ++ ",\n" ++
  c ++ ".width_byteena_a = 1,\n" ++
  c ++ ".width_byteena_b = 1;\n"


instRam2 params comp (we1:we2:sigs) =
 let  init = read (lookupParam params "init") :: [Integer]
      initFile = "ram_" ++ compStr comp ++ ".mif"
      dwidth = read (lookupParam params "dwidth") :: Int
      awidth = read (lookupParam params "awidth") :: Int

      (dbus, abus)   = splitAt (2*dwidth) sigs
      (abus1, abus2) = splitAt awidth abus
      (dbus1, dbus2) = splitAt dwidth dbus
      outs1          = map ((,) comp) [0..dwidth-1]
      outs2          = map ((,) comp) [dwidth..dwidth*2-1]
      c              = " " ++ compStr comp
 in
  "  altsyncram" ++ c ++ "(\n" ++
  "   .clock0 (clock),\n" ++

  "   .address_a (" ++ vBus abus1 ++ "),\n" ++
  "   .wren_a (" ++ wireStr we1 ++ "),\n" ++
  "   .data_a (" ++ vBus dbus1 ++ "),\n" ++
  "   .q_a (" ++ vBus outs1 ++ "),\n" ++

  "   .address_b (" ++ vBus abus2 ++ "),\n" ++
  "   .wren_b (" ++ wireStr we2 ++ "),\n" ++
  "   .data_b (" ++ vBus dbus2 ++ "),\n" ++
  "   .q_b (" ++ vBus outs2 ++ "),\n" ++

  "   .aclr0 (1'b0),\n" ++
  "   .aclr1 (1'b0),\n" ++
  "   .addressstall_a (1'b0),\n" ++
  "   .addressstall_b (1'b0),\n" ++
  "   .byteena_a (1'b1),\n" ++
  "   .byteena_b (1'b1),\n" ++
  "   .clock1 (1'b1),\n" ++
  "   .clocken0 (1'b1),\n" ++
  "   .clocken1 (1'b1),\n" ++
  "   .clocken2 (1'b1),\n" ++
  "   .clocken3 (1'b1),\n" ++
  "   .eccstatus (),\n" ++
  "   .rden_a (1'b1),\n" ++
  "   .rden_b (1'b1));\n" ++
  "  defparam\n" ++
  c ++ ".address_reg_b = \"CLOCK0\",\n" ++
  c ++ ".clock_enable_input_a = \"BYPASS\",\n" ++
  c ++ ".clock_enable_input_b = \"BYPASS\",\n" ++
  c ++ ".clock_enable_output_a = \"BYPASS\",\n" ++
  c ++ ".clock_enable_output_b = \"BYPASS\",\n" ++
  c ++ ".indata_reg_b = \"CLOCK0\",\n" ++
  (if null init then "" else c ++ ".init_file = \"" ++ initFile ++ "\",\n") ++
  c ++ ".lpm_type = \"altsyncram\",\n" ++
  c ++ ".numwords_a = " ++ show (2^awidth) ++ ",\n" ++
  c ++ ".numwords_b = " ++ show (2^awidth) ++ ",\n" ++
  c ++ ".operation_mode = \"BIDIR_DUAL_PORT\",\n" ++
  c ++ ".outdata_aclr_a = \"NONE\",\n" ++
  c ++ ".outdata_aclr_b = \"NONE\",\n" ++
  c ++ ".outdata_reg_a = \"UNREGISTERED\",\n" ++
  c ++ ".outdata_reg_b = \"UNREGISTERED\",\n" ++
  c ++ ".power_up_uninitialized = \"FALSE\",\n" ++
  c ++ ".read_during_write_mode_mixed_ports = \"DONT_CARE\",\n" ++
  c ++ ".read_during_write_mode_port_a = \"NEW_DATA_NO_NBE_READ\",\n" ++
  c ++ ".read_during_write_mode_port_b = \"NEW_DATA_NO_NBE_READ\",\n" ++
  c ++ ".widthad_a = " ++ show awidth ++ ",\n" ++
  c ++ ".widthad_b = " ++ show awidth ++ ",\n" ++
  c ++ ".width_a = " ++ show dwidth ++ ",\n" ++
  c ++ ".width_b = " ++ show dwidth ++ ",\n" ++
  c ++ ".width_byteena_a = 1,\n" ++
  c ++ ".width_byteena_b = 1,\n" ++
  c ++ ".wrcontrol_wraddress_reg_b = \"CLOCK0\";\n"
