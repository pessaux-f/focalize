require('dedukti')
dk_dk_machine_int = { }
require("cc")
require("dk_bool")
check_ext(dk_cc,'[ Lua ]  cc is undefined.')
check_ext(dk_dk_bool,'[ Lua ]  dk_bool is undefined.')
dk_dk_machine_int.B_c = app( app0(dk_cc.eT_c) , app0(dk_dk_bool.bool_c) )
dk_dk_machine_int.B_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_bool.bool_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.bool_c) end } }
require("dk_nat")
check_ext(dk_dk_nat,'[ Lua ]  dk_nat is undefined.')
dk_dk_machine_int.UNat_c = app( app0(dk_cc.eT_c) , app0(dk_dk_nat.Nat_c) )
dk_dk_machine_int.UNat_t = { tapp_f = dk_cc.eT_t ; tapp_a = dk_dk_nat.Nat_t ; tapp_ca = { clazy = function() return app0(dk_dk_nat.Nat_c) end } }
dk_dk_machine_int.UO_c = app0(dk_dk_nat.O_c)
dk_dk_machine_int.UO_t = dk_dk_nat.O_t
dk_dk_machine_int.US_c = app0(dk_dk_nat.S_c)
dk_dk_machine_int.US_t = dk_dk_nat.S_t
check_ext(dk_dk_machine_int,'[ Lua ]  dk_machine_int is undefined.')
dk_dk_machine_int.Mint_c = { cid = "dk_machine_int.Mint" ; args = { } }
dk_dk_machine_int.Mint_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (dummy) return app0(dk_cc.uT_c) end } }
dk_dk_machine_int.MInt_c = { clam_f = function (N_c) return app( app0(dk_cc.eT_c) , app( app0(dk_dk_machine_int.Mint_c) , N_c ) ) end }
dk_dk_machine_int.MInt_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tapp_f = dk_cc.eT_t ; tapp_a = { tapp_f = dk_dk_machine_int.Mint_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_ca = { clazy = function() return app( app0(dk_dk_machine_int.Mint_c) , N_c ) end } } end }
dk_dk_machine_int.O_c = { cid = "dk_machine_int.O" ; args = { } }
dk_dk_machine_int.O_t = { tbox_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , app0(dk_dk_machine_int.UO_c) ) end } }
dk_dk_machine_int.S0_c = { cid = "dk_machine_int.S0" ; args = { } }
dk_dk_machine_int.S0_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , app( app0(dk_dk_machine_int.US_c) , N_c ) ) end } end } }
dk_dk_machine_int.S1_c = { cid = "dk_machine_int.S1" ; args = { } }
dk_dk_machine_int.S1_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , app( app0(dk_dk_machine_int.US_c) , N_c ) ) end } end } }
dk_dk_machine_int.zero_c = { cid = "dk_machine_int.zero" ; args = { } }
dk_dk_machine_int.zero_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } }
dk_dk_machine_int.zero_c = { cid="dk_machine_int.zero" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_machine_int.UO" then
return app0(dk_dk_machine_int.O_c)
elseif y1.cid == "dk_machine_int.US" then
local N_c = y1.args[1]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app0(dk_dk_machine_int.zero_c) , N_c ) )
else
return nil
end
end }
dk_dk_machine_int.bound_c = { cid = "dk_machine_int.bound" ; args = { } }
dk_dk_machine_int.bound_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } }
dk_dk_machine_int.bound_c = { cid="dk_machine_int.bound" ; arity = 1 ; args = { } ; f = function(y1)
local y1 = force2(y1)
if y1.cid == "dk_machine_int.UO" then
return app0(dk_dk_machine_int.O_c)
elseif y1.cid == "dk_machine_int.US" then
local N_c = y1.args[1]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app0(dk_dk_machine_int.bound_c) , N_c ) )
else
return nil
end
end }
dk_dk_machine_int.downcast_c = { cid = "dk_machine_int.downcast" ; args = { } }
dk_dk_machine_int.downcast_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , app( app0(dk_dk_machine_int.US_c) , N_c ) ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } }
dk_dk_machine_int.downcast_c = { cid="dk_machine_int.downcast" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_machine_int.UO" then
local n_c = y2
return app0(dk_dk_machine_int.O_c)
else
if y2.cid == "dk_machine_int.S1" then
if y2.args[1].cid == "dk_machine_int.US" then
local N_c, n_c, dummy = y2.args[1].args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app0(dk_dk_machine_int.downcast_c) , N_c ) , n_c ) )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y2.args[1].cid == "dk_machine_int.US" then
local N_c, n_c, dummy = y2.args[1].args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.downcast_c) , N_c ) , n_c ) )
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.double_c = { clam_f = function (N_c) return { clam_f = function (n_c) return app( app( app0(dk_dk_machine_int.downcast_c) , N_c ) , app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , n_c ) ) end } end }
dk_dk_machine_int.double_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tapp_f = { tapp_f = dk_dk_machine_int.downcast_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.S0_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , n_c ) end } } end } end }
dk_dk_machine_int.succ_c = { cid = "dk_machine_int.succ" ; args = { } }
dk_dk_machine_int.succ_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } }
dk_dk_machine_int.succ_c = { cid="dk_machine_int.succ" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
return app0(dk_dk_machine_int.O_c)
elseif y2.cid == "dk_machine_int.S1" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.succ_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S0" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , n_c )
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.succ_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S0" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , n_c )
else
return nil
end
end
end }
dk_dk_machine_int.pred_c = { cid = "dk_machine_int.pred" ; args = { } }
dk_dk_machine_int.pred_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } }
dk_dk_machine_int.pred_c = { cid="dk_machine_int.pred" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
return app0(dk_dk_machine_int.O_c)
elseif y2.cid == "dk_machine_int.S0" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app0(dk_dk_machine_int.pred_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S1" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , n_c )
else
return nil
end
else
if y2.cid == "dk_machine_int.S0" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app0(dk_dk_machine_int.pred_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S1" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , n_c )
else
return nil
end
end
end }
dk_dk_machine_int.plus_c = { cid = "dk_machine_int.plus" ; args = { } }
dk_dk_machine_int.plus_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } end } }
dk_dk_machine_int.plus_c = { cid="dk_machine_int.plus" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
if y3.cid == "dk_machine_int.O" then
return app0(dk_dk_machine_int.O_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.succ_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) ) )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
else
return nil
end
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.succ_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) ) )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) )
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.complement_c = { cid = "dk_machine_int.complement" ; args = { } }
dk_dk_machine_int.complement_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } }
dk_dk_machine_int.complement_c = { cid="dk_machine_int.complement" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
return app0(dk_dk_machine_int.O_c)
elseif y2.cid == "dk_machine_int.S1" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.complement_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S0" then
local N_c, n_c = y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app0(dk_dk_machine_int.complement_c) , N_c ) , n_c ) )
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app0(dk_dk_machine_int.complement_c) , N_c ) , n_c ) )
elseif y2.cid == "dk_machine_int.S0" then
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app0(dk_dk_machine_int.complement_c) , N_c ) , n_c ) )
else
return nil
end
end
end }
dk_dk_machine_int.opp_c = { cid = "dk_machine_int.opp" ; args = { } }
dk_dk_machine_int.opp_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } }
dk_dk_machine_int.opp_c = { cid="dk_machine_int.opp" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
local N_c, n_c = y1, y2
return app( app( app0(dk_dk_machine_int.succ_c) , N_c ) , app( app( app0(dk_dk_machine_int.complement_c) , N_c ) , n_c ) )
end }
dk_dk_machine_int.sub_c = { cid = "dk_machine_int.sub" ; args = { } }
dk_dk_machine_int.sub_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } end } }
dk_dk_machine_int.sub_c = { cid="dk_machine_int.sub" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
local N_c, n_c, m_c = y1, y2, y3
return app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , app( app( app0(dk_dk_machine_int.opp_c) , N_c ) , m_c ) )
end }
dk_dk_machine_int.mult_c = { cid = "dk_machine_int.mult" ; args = { } }
dk_dk_machine_int.mult_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } end } end } }
dk_dk_machine_int.mult_c = { cid="dk_machine_int.mult" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
if y3.cid == "dk_machine_int.O" then
return app0(dk_dk_machine_int.O_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , m_c ) , n_c ) ) ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) ) )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.double_c) , app( app0(dk_dk_machine_int.US_c) , N_c ) ) , app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , m_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) ) )
else
return nil
end
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S1_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , m_c ) , n_c ) ) ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , n_c ) , m_c ) ) )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.double_c) , app( app0(dk_dk_machine_int.US_c) , N_c ) ) , app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app0(dk_dk_machine_int.S0_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.plus_c) , N_c ) , m_c ) , app( app( app0(dk_dk_machine_int.double_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.mult_c) , N_c ) , n_c ) , m_c ) ) ) )
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.equal_c = { cid = "dk_machine_int.equal" ; args = { } }
dk_dk_machine_int.equal_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app0(dk_dk_machine_int.B_c) end } end } end } }
dk_dk_machine_int.equal_c = { cid="dk_machine_int.equal" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
if y3.cid == "dk_machine_int.O" then
return app0(dk_dk_bool.true_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.equal_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app0(dk_dk_bool.false_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.equal_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app0(dk_dk_bool.false_c)
else
return nil
end
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.equal_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app0(dk_dk_bool.false_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.equal_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app0(dk_dk_bool.false_c)
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.unsigned_lt_c = { cid = "dk_machine_int.unsigned_lt" ; args = { } }
dk_dk_machine_int.unsigned_lt_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app0(dk_dk_machine_int.B_c) end } end } end } }
dk_dk_machine_int.unsigned_leq_c = { cid = "dk_machine_int.unsigned_leq" ; args = { } }
dk_dk_machine_int.unsigned_leq_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app0(dk_dk_machine_int.B_c) end } end } end } }
dk_dk_machine_int.unsigned_lt_c = { cid="dk_machine_int.unsigned_lt" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
if y3.cid == "dk_machine_int.O" then
return app0(dk_dk_bool.false_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
else
return nil
end
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.unsigned_leq_c = { cid="dk_machine_int.unsigned_leq" ; arity = 3 ; args = { } ; f = function(y1, y2, y3)
local y1 = force2(y1)
local y2 = force2(y2)
local y3 = force2(y3)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
if y3.cid == "dk_machine_int.O" then
return app0(dk_dk_bool.true_c)
else
return nil
end
elseif y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c = y3.args[1], y3.args[2], y2.args[1], y2.args[2]
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
else
return nil
end
else
return nil
end
else
if y2.cid == "dk_machine_int.S1" then
if y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , n_c ) , m_c )
else
return nil
end
elseif y2.cid == "dk_machine_int.S0" then
if y3.cid == "dk_machine_int.S0" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
elseif y3.cid == "dk_machine_int.S1" then
local N_c, m_c, dummy, n_c, dummy = y3.args[1], y3.args[2], y2.args[1], y2.args[2], y1
return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , n_c ) , m_c )
else
return nil
end
else
return nil
end
end
end }
dk_dk_machine_int.unsigned_gt_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app( app( app0(dk_dk_machine_int.unsigned_lt_c) , N_c ) , m_c ) , n_c ) end } end } end }
dk_dk_machine_int.unsigned_gt_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.unsigned_lt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } end } end } end }
dk_dk_machine_int.unsigned_geq_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app( app( app0(dk_dk_machine_int.unsigned_leq_c) , N_c ) , m_c ) , n_c ) end } end } end }
dk_dk_machine_int.unsigned_geq_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.unsigned_leq_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } end } end } end }
dk_dk_machine_int.positive_c = { cid = "dk_machine_int.positive" ; args = { } }
dk_dk_machine_int.positive_t = { tbox_cty = { cpi_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; cpi_f = function (N_c) return { cpi_cty = app( app0(dk_dk_machine_int.MInt_c) , N_c ) ; cpi_f = function (dummy) return app0(dk_dk_machine_int.B_c) end } end } }
dk_dk_machine_int.positive_c = { cid="dk_machine_int.positive" ; arity = 2 ; args = { } ; f = function(y1, y2)
local y1 = force2(y1)
local y2 = force2(y2)
if y1.cid == "dk_machine_int.UO" then
if y2.cid == "dk_machine_int.O" then
return app0(dk_dk_bool.true_c)
else
return nil
end
elseif y1.cid == "dk_machine_int.US" then
if y2.cid == "dk_machine_int.S1" then
if y2.args[1].cid == "dk_machine_int.UO" then
if y2.args[2].cid == "dk_machine_int.O" then
if y1.args[1].cid == "dk_machine_int.UO" then
return app0(dk_dk_bool.false_c)
else
local dummy = y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
else
local n_c, dummy = y2.args[2], y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
else
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
elseif y2.cid == "dk_machine_int.S0" then
if y2.args[1].cid == "dk_machine_int.UO" then
if y2.args[2].cid == "dk_machine_int.O" then
if y1.args[1].cid == "dk_machine_int.UO" then
return app0(dk_dk_bool.true_c)
else
local dummy = y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
else
local n_c, dummy = y2.args[2], y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
else
local N_c, n_c, dummy = y2.args[1], y2.args[2], y1.args[1]
return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c )
end
else
return nil
end
else
return nil
end
end }
dk_dk_machine_int.signed_leq_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app( app( app0(dk_dk_bool.iteb_c) , app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) , app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) ) ) , app0(dk_dk_bool.true_c) ) , app( app( app( app0(dk_dk_bool.iteb_c) , app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) , app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) ) ) , app0(dk_dk_bool.false_c) ) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.sub_c) , N_c ) , m_c ) , n_c ) ) ) ) end } end } end }
dk_dk_machine_int.signed_leq_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_bool.and_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.positive_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) end } } ; tapp_a = { tapp_f = dk_dk_bool.not_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.positive_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) end } } ; tapp_ca = { clazy = function() return app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) end } } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) , app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) ) end } } ; tapp_a = dk_dk_bool.true_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.true_c) end } } ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_bool.iteb_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_bool.and_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.positive_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) end } } ; tapp_a = { tapp_f = dk_dk_bool.not_t ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.positive_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) end } } ; tapp_ca = { clazy = function() return app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) end } } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) , app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) ) end } } ; tapp_a = dk_dk_bool.false_t ; tapp_ca = { clazy = function() return app0(dk_dk_bool.false_c) end } } ; tapp_a = { tapp_f = { tapp_f = dk_dk_machine_int.positive_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.sub_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_machine_int.sub_c) , N_c ) , m_c ) , n_c ) end } } ; tapp_ca = { clazy = function() return app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.sub_c) , N_c ) , m_c ) , n_c ) ) end } } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_bool.iteb_c) , app( app( app0(dk_dk_bool.and_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , n_c ) ) , app( app0(dk_dk_bool.not_c) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , m_c ) ) ) ) , app0(dk_dk_bool.false_c) ) , app( app( app0(dk_dk_machine_int.positive_c) , N_c ) , app( app( app( app0(dk_dk_machine_int.sub_c) , N_c ) , m_c ) , n_c ) ) ) end } } end } end } end }
dk_dk_machine_int.signed_geq_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app( app( app0(dk_dk_machine_int.signed_leq_c) , N_c ) , m_c ) , n_c ) end } end } end }
dk_dk_machine_int.signed_geq_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.signed_leq_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } end } end } end }
dk_dk_machine_int.signed_lt_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app0(dk_dk_bool.not_c) , app( app( app( app0(dk_dk_machine_int.signed_geq_c) , N_c ) , m_c ) , n_c ) ) end } end } end }
dk_dk_machine_int.signed_lt_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = dk_dk_bool.not_t ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.signed_geq_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_machine_int.signed_geq_c) , N_c ) , m_c ) , n_c ) end } } end } end } end }
dk_dk_machine_int.signed_gt_c = { clam_f = function (N_c) return { clam_f = function (n_c) return { clam_f = function (m_c) return app( app0(dk_dk_bool.not_c) , app( app( app( app0(dk_dk_machine_int.signed_leq_c) , N_c ) , m_c ) , n_c ) ) end } end } end }
dk_dk_machine_int.signed_gt_t = { tlam_tty = dk_dk_machine_int.UNat_t ; tlam_cty = { clazy = function() return app0(dk_dk_machine_int.UNat_c) end } ; tlam_f =  function (N_t, N_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (n_t, n_c) return { tlam_tty = { tapp_f = dk_dk_machine_int.MInt_t ; tapp_a = N_t ; tapp_ca = N_c } ; tlam_cty = { clazy = function() return app( app0(dk_dk_machine_int.MInt_c) , N_c ) end } ; tlam_f =  function (m_t, m_c) return { tapp_f = dk_dk_bool.not_t ; tapp_a = { tapp_f = { tapp_f = { tapp_f = dk_dk_machine_int.signed_leq_t ; tapp_a = N_t ; tapp_ca = N_c } ; tapp_a = m_t ; tapp_ca = m_c } ; tapp_a = n_t ; tapp_ca = n_c } ; tapp_ca = { clazy = function() return app( app( app( app0(dk_dk_machine_int.signed_leq_c) , N_c ) , m_c ) , n_c ) end } } end } end } end }
