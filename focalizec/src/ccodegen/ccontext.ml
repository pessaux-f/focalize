open Env
open Format

type context = 
    { mutable c_env : CGenEnv.t;
      c_src : formatter;
      c_hdr : formatter }
