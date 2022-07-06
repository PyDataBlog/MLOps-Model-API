[00000]  {:noop}
[00001]  {:noop}
[00002]  {:out, {:value, 'Welcome to the Synacor Challenge!'}}
[00068]  {:out_newline, {:value, :newline}}
[00070]  {:out, {:value, 'Please record your progress by putting codes like'}}
[00168]  {:out_newline, {:value, :newline}}
[00170]  {:out, {:value, 'this one into the challenge website: OdqvynwboxQF'}}
[00268]  {:out_newline, {:value, :newline}}
[00270]  {:out_newline, {:value, :newline}}
[00272]  {:out, {:value, 'Executing self-test...'}}
[00316]  {:out_newline, {:value, :newline}}
[00318]  {:out_newline, {:value, :newline}}
[00320]  {:jmp, {:value, 347}}
[00322]  {:out, {:value, 'jmp fails'}}
[00340]  {:out_newline, {:value, :newline}}
[00342]  {:halt}
[00343]  {:noop}
[00344]  {:noop}
[00345]  {:noop}
[00346]  {:noop}
[00347]  {:noop}
[00348]  {:noop}
[00349]  {:noop}
[00350]  {:noop}
[00351]  {:noop}
[00352]  {:jmp, {:value, 358}}
[00354]  {:jmp, {:value, 368}}
[00356]  {:jmp, {:value, 397}}
[00358]  {:jmp, {:value, 484}}
[00360]  {:jmp, {:value, 424}}
[00362]  {:jmp, {:value, 453}}
[00364]  {:noop}
[00365]  {:noop}
[00366]  {:noop}
[00367]  {:noop}
[00368]  {:out, {:value, 'jmp lands -2'}}
[00392]  {:out_newline, {:value, :newline}}
[00394]  {:halt}
[00395]  {:noop}
[00396]  {:noop}
[00397]  {:out, {:value, 'jmp lands -1'}}
[00421]  {:out_newline, {:value, :newline}}
[00423]  {:halt}
[00424]  {:noop}
[00425]  {:noop}
[00426]  {:out, {:value, 'jmp lands +1'}}
[00450]  {:out_newline, {:value, :newline}}
[00452]  {:halt}
[00453]  {:noop}
[00454]  {:noop}
[00455]  {:noop}
[00456]  {:noop}
[00457]  {:out, {:value, 'jmp lands +2'}}
[00481]  {:out_newline, {:value, :newline}}
[00483]  {:halt}
[00484]  {:jt, {:value, 0}, {:value, 1074}}
[00487]  {:jf, {:value, 1}, {:value, 1074}}
[00490]  {:jt, {:value, 1}, {:value, 495}}
[00493]  {:jmp, {:value, 1074}}
[00495]  {:jf, {:value, 0}, {:value, 500}}
[00498]  {:jmp, {:value, 1074}}
[00500]  {:jt, {:reg, 0}, {:value, 1093}}
[00503]  {:jt, {:reg, 1}, {:value, 1093}}
[00506]  {:jt, {:reg, 2}, {:value, 1093}}
[00509]  {:jt, {:reg, 3}, {:value, 1093}}
[00512]  {:jt, {:reg, 4}, {:value, 1093}}
[00515]  {:jt, {:reg, 5}, {:value, 1093}}
[00518]  {:jt, {:reg, 6}, {:value, 1093}}
[00521]  {:jt, {:reg, 7}, {:value, 1093}}
[00524]  {:set, {:reg, 0}, {:value, 1}}
[00527]  {:jf, {:reg, 0}, {:value, 1118}}
[00530]  {:set, {:reg, 0}, {:value, 0}}
[00533]  {:jt, {:reg, 0}, {:value, 1118}}
[00536]  {:add, {:reg, 0}, {:value, 1}, {:value, 1}}
[00540]  {:jt, {:reg, 0}, {:value, 564}}
[00543]  {:out, {:value, 'no add op'}}
[00561]  {:out_newline, {:value, :newline}}
[00563]  {:halt}
[00564]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 2}}
[00568]  {:jt, {:reg, 1}, {:value, 590}}
[00571]  {:out, {:value, 'no eq op'}}
[00587]  {:out_newline, {:value, :newline}}
[00589]  {:halt}
[00590]  {:push, {:reg, 0}}
[00592]  {:push, {:reg, 1}}
[00594]  {:pop, {:reg, 0}}
[00596]  {:pop, {:reg, 1}}
[00598]  {:eq, {:reg, 2}, {:reg, 1}, {:value, 2}}
[00602]  {:jf, {:reg, 2}, {:value, 1158}}
[00605]  {:eq, {:reg, 2}, {:reg, 0}, {:value, 1}}
[00609]  {:jf, {:reg, 2}, {:value, 1158}}
[00612]  {:gt, {:reg, 2}, {:reg, 1}, {:reg, 0}}
[00616]  {:jf, {:reg, 2}, {:value, 1139}}
[00619]  {:gt, {:reg, 2}, {:reg, 0}, {:reg, 1}}
[00623]  {:jt, {:reg, 2}, {:value, 1139}}
[00626]  {:gt, {:reg, 2}, {:value, 42}, {:value, 42}}
[00630]  {:jt, {:reg, 2}, {:value, 1139}}
[00633]  {:and, {:reg, 0}, {:value, 28912}, {:value, 19626}}
[00637]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 16544}}
[00641]  {:jf, {:reg, 1}, {:value, 1177}}
[00644]  {:or, {:reg, 1}, {:value, 28912}, {:value, 19626}}
[00648]  {:eq, {:reg, 0}, {:reg, 1}, {:value, 31994}}
[00652]  {:jt, {:reg, 0}, {:value, 684}}
[00655]  {:out, {:value, 'no bitwise or'}}
[00681]  {:out_newline, {:value, :newline}}
[00683]  {:halt}
[00684]  {:not, {:reg, 0}, {:value, 0}}
[00687]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32767}}
[00691]  {:jf, {:reg, 1}, {:value, 1208}}
[00694]  {:not, {:reg, 0}, {:value, 21845}}
[00697]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 10922}}
[00701]  {:jf, {:reg, 1}, {:value, 1208}}
[00704]  {:call, {:value, 1285}}
[00706]  {:jmp, {:value, 1289}}
[00708]  {:pop, {:reg, 0}}
[00710]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 708}}
[00714]  {:jt, {:reg, 1}, {:value, 1289}}
[00717]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 706}}
[00721]  {:jf, {:reg, 1}, {:value, 1289}}
[00724]  {:set, {:reg, 0}, {:value, 1287}}
[00727]  {:call, {:reg, 0}}
[00729]  {:jmp, {:value, 1289}}
[00731]  {:pop, {:reg, 0}}
[00733]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 731}}
[00737]  {:jt, {:reg, 1}, {:value, 1289}}
[00740]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 729}}
[00744]  {:jf, {:reg, 1}, {:value, 1289}}
[00747]  {:add, {:reg, 0}, {:value, 32767}, {:value, 32767}}
[00751]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32766}}
[00755]  {:jf, {:reg, 1}, {:value, 1312}}
[00758]  {:eq, {:reg, 1}, {:value, 32766}, {:reg, 0}}
[00762]  {:jf, {:reg, 1}, {:value, 1312}}
[00765]  {:add, {:reg, 0}, {:value, 16384}, {:value, 16384}}
[00769]  {:jt, {:reg, 0}, {:value, 1312}}
[00772]  {:add, {:reg, 0}, {:value, 16384}, {:value, 16384}}
[00776]  {:jt, {:reg, 0}, {:value, 1312}}
[00779]  {:mult, {:reg, 0}, {:value, 6}, {:value, 9}}
[00783]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 42}}
[00787]  {:jt, {:reg, 1}, {:value, 1381}}
[00790]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 54}}
[00794]  {:jf, {:reg, 1}, {:value, 1414}}
[00797]  {:mult, {:reg, 0}, {:value, 12345}, {:value, 32123}}
[00801]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 99}}
[00805]  {:jf, {:reg, 1}, {:value, 1312}}
[00808]  {:mod, {:reg, 0}, {:value, 6}, {:value, 3}}
[00812]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 0}}
[00816]  {:jf, {:reg, 1}, {:value, 1437}}
[00819]  {:mod, {:reg, 0}, {:value, 70}, {:value, 6}}
[00823]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 4}}
[00827]  {:jf, {:reg, 1}, {:value, 1437}}
[00830]  {:mult, {:reg, 0}, {:value, 32766}, {:value, 15}}
[00834]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32738}}
[00838]  {:jf, {:reg, 1}, {:value, 1312}}
[00841]  {:jmp, {:value, 845}}
[00843]  {:unknown, [30000]}
[00844]  {:unknown, [10000]}
[00845]  {:rmem, {:reg, 0}, {:value, 843}}
[00848]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 20000}}
[00852]  {:jf, {:reg, 1}, {:value, 1239}}
[00855]  {:add, {:reg, 2}, {:value, 843}, {:value, 1}}
[00859]  {:rmem, {:reg, 0}, {:reg, 2}}
[00862]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 10000}}
[00866]  {:jf, {:reg, 1}, {:value, 1239}}
[00869]  {:set, {:reg, 0}, {:value, 843}}
[00872]  {:wmem, {:reg, 0}, {:value, 30000}}
[00875]  {:rmem, {:reg, 2}, {:reg, 0}}
[00878]  {:eq, {:reg, 1}, {:reg, 2}, {:value, 30000}}
[00882]  {:jf, {:reg, 1}, {:value, 1262}}
[00885]  {:call, {:value, 1723}}
[00887]  {:rmem, {:reg, 0}, {:value, 6068}}
[00890]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 11}}
[00894]  {:jf, {:reg, 1}, {:value, 1239}}
[00897]  {:add, {:reg, 2}, {:value, 6068}, {:value, 1}}
[00901]  {:rmem, {:reg, 0}, {:reg, 2}}
[00904]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 116}}
[00908]  {:jf, {:reg, 1}, {:value, 1239}}
[00911]  {:wmem, {:reg, 2}, {:value, 84}}
[00914]  {:rmem, {:reg, 0}, {:reg, 2}}
[00917]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 116}}
[00921]  {:jt, {:reg, 1}, {:value, 1262}}
[00924]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 84}}
[00928]  {:jf, {:reg, 1}, {:value, 1262}}
[00931]  {:wmem, {:value, 937}, {:value, 21}}
[00934]  {:wmem, {:value, 938}, {:value, 7}}
[00937]  {:noop}
[00938]  {:jt, {:value, 19}, {:value, 978}}
[00941]  {:out, {:value, 'wmem opwrite fail'}}
[00975]  {:out_newline, {:value, :newline}}
[00977]  {:halt}
[00978]  {:add, {:reg, 1}, {:value, 10}, {:value, 6080}}
[00982]  {:add, {:reg, 1}, {:reg, 1}, {:value, 1}}
[00986]  {:rmem, {:reg, 2}, {:value, 6116}}
[00989]  {:add, {:reg, 2}, {:reg, 2}, {:value, 6116}}
[00993]  {:set, {:reg, 0}, {:value, 6116}}
[00996]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[01000]  {:gt, {:reg, 3}, {:reg, 0}, {:reg, 2}}
[01004]  {:jt, {:reg, 3}, {:value, 1023}}
[01007]  {:rmem, {:reg, 4}, {:reg, 0}}
[01010]  {:wmem, {:reg, 1}, {:reg, 4}}
[01013]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[01017]  {:add, {:reg, 1}, {:reg, 1}, {:value, 1}}
[01021]  {:jmp, {:value, 1000}}
[01023]  {:rmem, {:reg, 0}, {:value, 6080}}
[01026]  {:rmem, {:reg, 1}, {:value, 6099}}
[01029]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[01033]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[01037]  {:wmem, {:value, 6080}, {:reg, 0}}
[01040]  {:wmem, {:value, 6099}, {:value, 44}}
[01043]  {:set, {:reg, 0}, {:value, 6080}}
[01046]  {:call, {:value, 1518}}
[01048]  {:push, {:reg, 0}}
[01050]  {:push, {:reg, 1}}
[01052]  {:push, {:reg, 2}}
[01054]  {:set, {:reg, 0}, {:value, 26851}}
[01057]  {:set, {:reg, 1}, {:value, 1531}}
[01060]  {:add, {:reg, 2}, {:value, 16353}, {:value, 5658}}
[01064]  {:call, {:value, 1458}}
[01066]  {:pop, {:reg, 2}}
[01068]  {:pop, {:reg, 1}}
[01070]  {:pop, {:reg, 0}}
[01072]  {:jmp, {:value, 2734}}
[01074]  {:out, {:value, 'no jt/jf'}}
[01090]  {:out_newline, {:value, :newline}}
[01092]  {:halt}
[01093]  {:out, {:value, 'nonzero reg'}}
[01115]  {:out_newline, {:value, :newline}}
[01117]  {:halt}
[01118]  {:out, {:value, 'no set op'}}
[01136]  {:out_newline, {:value, :newline}}
[01138]  {:halt}
[01139]  {:out, {:value, 'no gt op'}}
[01155]  {:out_newline, {:value, :newline}}
[01157]  {:halt}
[01158]  {:out, {:value, 'no stack'}}
[01174]  {:out_newline, {:value, :newline}}
[01176]  {:halt}
[01177]  {:out, {:value, 'no bitwise and'}}
[01205]  {:out_newline, {:value, :newline}}
[01207]  {:halt}
[01208]  {:out, {:value, 'no bitwise not'}}
[01236]  {:out_newline, {:value, :newline}}
[01238]  {:halt}
[01239]  {:out, {:value, 'no rmem op'}}
[01259]  {:out_newline, {:value, :newline}}
[01261]  {:halt}
[01262]  {:out, {:value, 'no wmem op'}}
[01282]  {:out_newline, {:value, :newline}}
[01284]  {:halt}
[01285]  {:jmp, {:value, 708}}
[01287]  {:jmp, {:value, 731}}
[01289]  {:out, {:value, 'no call op'}}
[01309]  {:out_newline, {:value, :newline}}
[01311]  {:halt}
[01312]  {:out, {:value, 'no modulo math during add or mult'}}
[01378]  {:out_newline, {:value, :newline}}
[01380]  {:halt}
[01381]  {:out, {:value, 'not hitchhiking'}}
[01411]  {:out_newline, {:value, :newline}}
[01413]  {:halt}
[01414]  {:out, {:value, 'no mult op'}}
[01434]  {:out_newline, {:value, :newline}}
[01436]  {:halt}
[01437]  {:out, {:value, 'no mod op'}}
[01455]  {:out_newline, {:value, :newline}}
[01457]  {:halt}
[01458]  {:push, {:reg, 0}}				# Write memory to output
[01460]  {:push, {:reg, 3}}				# r0 holds the memory offset
[01462]  {:push, {:reg, 4}}				# r1 holds the print function to use (1528)
[01464]  {:push, {:reg, 5}}				# first byte of memory is number of chars to write
[01466]  {:push, {:reg, 6}}
[01468]  {:set, {:reg, 6}, {:reg, 0}}
[01471]  {:set, {:reg, 5}, {:reg, 1}}
[01474]  {:rmem, {:reg, 4}, {:reg, 0}}
[01477]  {:set, {:reg, 1}, {:value, 0}}
[01480]  {:add, {:reg, 3}, {:value, 1}, {:reg, 1}}
[01484]  {:gt, {:reg, 0}, {:reg, 3}, {:reg, 4}}
[01488]  {:jt, {:reg, 0}, {:value, 1507}}
[01491]  {:add, {:reg, 3}, {:reg, 3}, {:reg, 6}}
[01495]  {:rmem, {:reg, 0}, {:reg, 3}}
[01498]  {:call, {:reg, 5}}				# write char to output
[01500]  {:add, {:reg, 1}, {:reg, 1}, {:value, 1}}
[01504]  {:jt, {:reg, 1}, {:value, 1480}}
[01507]  {:pop, {:reg, 6}}
[01509]  {:pop, {:reg, 5}}
[01511]  {:pop, {:reg, 4}}
[01513]  {:pop, {:reg, 3}}
[01515]  {:pop, {:reg, 0}}
[01517]  {:ret}
[01518]  {:push, {:reg, 1}}				# Pass output function to write function
[01520]  {:set, {:reg, 1}, {:value, 1528}}
[01523]  {:call, {:value, 1458}}
[01525]  {:pop, {:reg, 1}}
[01527]  {:ret}
[01528]  {:out, {:reg, 0}}				# write char to output
[01530]  {:ret}
[01531]  {:push, {:reg, 1}}				# decrypt wrapper
[01533]  {:set, {:reg, 1}, {:reg, 2}}
[01536]  {:call, {:value, 2125}}
[01538]  {:out, {:reg, 0}}
[01540]  {:pop, {:reg, 1}}
[01542]  {:ret}
[01543]  {:push, {:reg, 1}}
[01545]  {:push, {:reg, 3}}
[01547]  {:rmem, {:reg, 3}, {:reg, 0}}
[01550]  {:jf, {:reg, 3}, {:value, 1563}}
[01553]  {:call, {:value, 1458}}
[01555]  {:jt, {:reg, 1}, {:value, 1563}}
[01558]  {:set, {:reg, 0}, {:reg, 2}}
[01561]  {:jmp, {:value, 1566}}
[01563]  {:set, {:reg, 0}, {:value, 32767}}
[01566]  {:pop, {:reg, 3}}
[01568]  {:pop, {:reg, 1}}
[01570]  {:ret}
[01571]  {:push, {:reg, 1}}
[01573]  {:push, {:reg, 2}}
[01575]  {:set, {:reg, 2}, {:reg, 1}}
[01578]  {:set, {:reg, 1}, {:value, 1605}}
[01581]  {:call, {:value, 1543}}
[01583]  {:pop, {:reg, 2}}
[01585]  {:pop, {:reg, 1}}
[01587]  {:ret}
[01588]  {:push, {:reg, 1}}
[01590]  {:push, {:reg, 2}}
[01592]  {:set, {:reg, 2}, {:reg, 1}}
[01595]  {:set, {:reg, 1}, {:value, 1648}}
[01598]  {:call, {:value, 1543}}
[01600]  {:pop, {:reg, 2}}
[01602]  {:pop, {:reg, 1}}
[01604]  {:ret}
[01605]  {:eq, {:reg, 0}, {:reg, 0}, {:reg, 2}}
[01609]  {:jf, {:reg, 0}, {:value, 1618}}
[01612]  {:set, {:reg, 2}, {:reg, 1}}
[01615]  {:set, {:reg, 1}, {:value, 32767}}
[01618]  {:ret}
[01619]  {:push, {:reg, 3}}
[01621]  {:add, {:reg, 3}, {:reg, 2}, {:value, 1}}
[01625]  {:add, {:reg, 3}, {:reg, 3}, {:reg, 1}}
[01629]  {:rmem, {:reg, 3}, {:reg, 3}}
[01632]  {:eq, {:reg, 3}, {:reg, 0}, {:reg, 3}}
[01636]  {:jt, {:reg, 3}, {:value, 1645}}
[01639]  {:set, {:reg, 2}, {:reg, 1}}
[01642]  {:set, {:reg, 1}, {:value, 32767}}
[01645]  {:pop, {:reg, 3}}
[01647]  {:ret}
[01648]  {:push, {:reg, 1}}
[01650]  {:set, {:reg, 1}, {:reg, 2}}
[01653]  {:call, {:value, 1667}}
[01655]  {:pop, {:reg, 1}}
[01657]  {:jf, {:reg, 0}, {:value, 1666}}
[01660]  {:set, {:reg, 2}, {:reg, 1}}
[01663]  {:set, {:reg, 1}, {:value, 32767}}
[01666]  {:ret}
[01667]  {:push, {:reg, 1}}
[01669]  {:push, {:reg, 2}}
[01671]  {:push, {:reg, 3}}
[01673]  {:push, {:reg, 4}}
[01675]  {:rmem, {:reg, 3}, {:reg, 0}}
[01678]  {:rmem, {:reg, 4}, {:reg, 1}}
[01681]  {:eq, {:reg, 2}, {:reg, 3}, {:reg, 4}}
[01685]  {:jf, {:reg, 2}, {:value, 1711}}
[01688]  {:or, {:reg, 2}, {:reg, 3}, {:reg, 4}}
[01692]  {:jf, {:reg, 2}, {:value, 1706}}
[01695]  {:set, {:reg, 2}, {:reg, 1}}
[01698]  {:set, {:reg, 1}, {:value, 1619}}
[01701]  {:call, {:value, 1458}}
[01703]  {:jf, {:reg, 1}, {:value, 1711}}
[01706]  {:set, {:reg, 0}, {:value, 1}}
[01709]  {:jmp, {:value, 1714}}
[01711]  {:set, {:reg, 0}, {:value, 0}}
[01714]  {:pop, {:reg, 4}}
[01716]  {:pop, {:reg, 3}}
[01718]  {:pop, {:reg, 2}}
[01720]  {:pop, {:reg, 1}}
[01722]  {:ret}
[01723]  {:push, {:reg, 0}}
[01725]  {:push, {:reg, 1}}
[01727]  {:set, {:reg, 1}, {:value, 6068}}
[01730]  {:rmem, {:reg, 0}, {:reg, 1}}
[01733]  {:push, {:reg, 1}}
[01735]  {:mult, {:reg, 1}, {:reg, 1}, {:reg, 1}}
[01739]  {:call, {:value, 2125}}
[01741]  {:set, {:reg, 1}, {:value, 16724}}
[01744]  {:call, {:value, 2125}}
[01746]  {:pop, {:reg, 1}}
[01748]  {:wmem, {:reg, 1}, {:reg, 0}}
[01751]  {:add, {:reg, 1}, {:reg, 1}, {:value, 1}}
[01755]  {:eq, {:reg, 0}, {:value, 30050}, {:reg, 1}}
[01759]  {:jf, {:reg, 0}, {:value, 1730}}
[01762]  {:pop, {:reg, 1}}
[01764]  {:pop, {:reg, 0}}
[01766]  {:ret}
[01767]  {:push, {:reg, 0}}				# Read user input
[01769]  {:push, {:reg, 2}}
[01771]  {:push, {:reg, 3}}
[01773]  {:push, {:reg, 4}}
[01775]  {:push, {:reg, 5}}
[01777]  {:add, {:reg, 2}, {:reg, 1}, {:reg, 0}}
[01781]  {:set, {:reg, 0}, {:reg, 1}}
[01784]  {:set, {:reg, 5}, {:value, 0}}
[01787]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[01791]  {:gt, {:reg, 3}, {:reg, 0}, {:reg, 2}}
[01795]  {:jt, {:reg, 3}, {:value, 1816}}
[01798]  {:in, {:reg, 4}}				# Main prompt read
[01800]  {:eq, {:reg, 3}, {:reg, 4}, {:value, 10}}				# check for newline (end of input)
[01804]  {:jt, {:reg, 3}, {:value, 1816}}
[01807]  {:wmem, {:reg, 0}, {:reg, 4}}
[01810]  {:add, {:reg, 5}, {:reg, 5}, {:value, 1}}
[01814]  {:jmp, {:value, 1787}}
[01816]  {:wmem, {:reg, 1}, {:reg, 5}}
[01819]  {:eq, {:reg, 3}, {:reg, 4}, {:value, 10}}
[01823]  {:jt, {:reg, 3}, {:value, 1830}}
[01826]  {:in, {:reg, 4}}
[01828]  {:jmp, {:value, 1819}}
[01830]  {:pop, {:reg, 5}}
[01832]  {:pop, {:reg, 4}}
[01834]  {:pop, {:reg, 3}}
[01836]  {:pop, {:reg, 2}}
[01838]  {:pop, {:reg, 0}}
[01840]  {:ret}
[01841]  {:push, {:reg, 3}}				# Print island teleport code
[01843]  {:push, {:reg, 4}}
[01845]  {:push, {:reg, 5}}
[01847]  {:push, {:reg, 6}}
[01849]  {:set, {:reg, 6}, {:value, 1}}
[01852]  {:add, {:reg, 4}, {:reg, 3}, {:reg, 6}}
[01856]  {:rmem, {:reg, 4}, {:reg, 4}}
[01859]  {:add, {:reg, 5}, {:value, 6125}, {:reg, 6}}
[01863]  {:wmem, {:reg, 5}, {:reg, 4}}
[01866]  {:add, {:reg, 6}, {:reg, 6}, {:value, 1}}
[01870]  {:rmem, {:reg, 5}, {:value, 6125}}
[01873]  {:gt, {:reg, 4}, {:reg, 6}, {:reg, 5}}
[01877]  {:jf, {:reg, 4}, {:value, 1852}}
[01880]  {:set, {:reg, 3}, {:value, 0}}
[01883]  {:set, {:reg, 4}, {:value, 0}}
[01886]  {:rmem, {:reg, 5}, {:value, 6125}}
[01889]  {:mod, {:reg, 5}, {:reg, 4}, {:reg, 5}}
[01893]  {:add, {:reg, 5}, {:reg, 5}, {:value, 6125}}
[01897]  {:add, {:reg, 5}, {:reg, 5}, {:value, 1}}
[01901]  {:rmem, {:reg, 6}, {:reg, 5}}
[01904]  {:mult, {:reg, 6}, {:reg, 6}, {:value, 5249}}
[01908]  {:add, {:reg, 6}, {:reg, 6}, {:value, 12345}}
[01912]  {:wmem, {:reg, 5}, {:reg, 6}}
[01915]  {:push, {:reg, 0}}
[01917]  {:push, {:reg, 1}}
[01919]  {:set, {:reg, 1}, {:reg, 6}}
[01922]  {:call, {:value, 2125}}
[01924]  {:set, {:reg, 6}, {:reg, 0}}
[01927]  {:pop, {:reg, 1}}
[01929]  {:pop, {:reg, 0}}
[01931]  {:rmem, {:reg, 5}, {:reg, 1}}
[01934]  {:mod, {:reg, 6}, {:reg, 6}, {:reg, 5}}
[01938]  {:add, {:reg, 6}, {:reg, 6}, {:value, 1}}
[01942]  {:gt, {:reg, 5}, {:reg, 6}, {:reg, 2}}
[01946]  {:jt, {:reg, 5}, {:value, 1952}}
[01949]  {:set, {:reg, 3}, {:value, 1}}
[01952]  {:add, {:reg, 6}, {:reg, 6}, {:reg, 1}}
[01956]  {:rmem, {:reg, 6}, {:reg, 6}}
[01959]  {:add, {:reg, 4}, {:reg, 4}, {:value, 1}}
[01963]  {:add, {:reg, 5}, {:reg, 4}, {:value, 6129}}
[01967]  {:wmem, {:reg, 5}, {:reg, 6}}
[01970]  {:rmem, {:reg, 5}, {:value, 6129}}
[01973]  {:eq, {:reg, 5}, {:reg, 4}, {:reg, 5}}
[01977]  {:jf, {:reg, 5}, {:value, 1886}}
[01980]  {:jf, {:reg, 3}, {:value, 1880}}
[01983]  {:push, {:reg, 0}}
[01985]  {:set, {:reg, 0}, {:value, 6129}}
[01988]  {:call, {:value, 1518}}
[01990]  {:pop, {:reg, 0}}
[01992]  {:pop, {:reg, 6}}
[01994]  {:pop, {:reg, 5}}
[01996]  {:pop, {:reg, 4}}
[01998]  {:pop, {:reg, 3}}
[02000]  {:ret}
[02001]  {:push, {:reg, 0}}
[02003]  {:push, {:reg, 1}}
[02005]  {:push, {:reg, 2}}
[02007]  {:push, {:reg, 3}}
[02009]  {:push, {:reg, 4}}
[02011]  {:push, {:reg, 5}}
[02013]  {:set, {:reg, 2}, {:value, 1}}
[02016]  {:set, {:reg, 5}, {:value, 0}}
[02019]  {:jf, {:reg, 0}, {:value, 2092}}
[02022]  {:eq, {:reg, 4}, {:reg, 2}, {:value, 10000}}
[02026]  {:set, {:reg, 3}, {:reg, 0}}
[02029]  {:jt, {:reg, 4}, {:value, 2040}}
[02032]  {:mult, {:reg, 1}, {:reg, 2}, {:value, 10}}
[02036]  {:mod, {:reg, 3}, {:reg, 0}, {:reg, 1}}
[02040]  {:set, {:reg, 4}, {:value, 0}}
[02043]  {:mult, {:reg, 2}, {:reg, 2}, {:value, 32767}}
[02047]  {:jf, {:reg, 3}, {:value, 2060}}
[02050]  {:add, {:reg, 4}, {:reg, 4}, {:value, 1}}
[02054]  {:add, {:reg, 3}, {:reg, 3}, {:reg, 2}}
[02058]  {:jmp, {:value, 2047}}
[02060]  {:mult, {:reg, 2}, {:reg, 2}, {:value, 32767}}
[02064]  {:mult, {:reg, 3}, {:reg, 4}, {:reg, 2}}
[02068]  {:mult, {:reg, 3}, {:reg, 3}, {:value, 32767}}
[02072]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 3}}
[02076]  {:add, {:reg, 4}, {:reg, 4}, {:value, 48}}
[02080]  {:mult, {:reg, 2}, {:reg, 2}, {:value, 10}}
[02084]  {:add, {:reg, 5}, {:reg, 5}, {:value, 1}}
[02088]  {:push, {:reg, 4}}
[02090]  {:jmp, {:value, 2019}}
[02092]  {:jt, {:reg, 5}, {:value, 2099}}
[02095]  {:out, {:value, '0'}}
[02097]  {:jmp, {:value, 2112}}
[02099]  {:jf, {:reg, 5}, {:value, 2112}}
[02102]  {:pop, {:reg, 0}}
[02104]  {:out, {:reg, 0}}
[02106]  {:add, {:reg, 5}, {:reg, 5}, {:value, 32767}}
[02110]  {:jmp, {:value, 2099}}
[02112]  {:pop, {:reg, 5}}
[02114]  {:pop, {:reg, 4}}
[02116]  {:pop, {:reg, 3}}
[02118]  {:pop, {:reg, 2}}
[02120]  {:pop, {:reg, 1}}
[02122]  {:pop, {:reg, 0}}
[02124]  {:ret}
[02125]  {:push, {:reg, 1}}				# decrypt function
[02127]  {:push, {:reg, 2}}
[02129]  {:and, {:reg, 2}, {:reg, 0}, {:reg, 1}}
[02133]  {:not, {:reg, 2}, {:reg, 2}}
[02136]  {:or, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02140]  {:and, {:reg, 0}, {:reg, 0}, {:reg, 2}}
[02144]  {:pop, {:reg, 2}}
[02146]  {:pop, {:reg, 1}}
[02148]  {:ret}
[02149]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02153]  {:gt, {:reg, 1}, {:reg, 1}, {:reg, 0}}
[02157]  {:ret}
[02158]  {:push, {:reg, 2}}
[02160]  {:gt, {:reg, 2}, {:reg, 1}, {:reg, 0}}
[02164]  {:mult, {:reg, 1}, {:reg, 1}, {:value, 32767}}
[02168]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02172]  {:set, {:reg, 1}, {:reg, 2}}
[02175]  {:pop, {:reg, 2}}
[02177]  {:ret}
[02178]  {:jf, {:reg, 0}, {:value, 2241}}
[02181]  {:jf, {:reg, 1}, {:value, 2241}}
[02184]  {:push, {:reg, 2}}
[02186]  {:push, {:reg, 3}}
[02188]  {:gt, {:reg, 2}, {:reg, 1}, {:reg, 0}}
[02192]  {:jt, {:reg, 2}, {:value, 2204}}
[02195]  {:set, {:reg, 2}, {:reg, 0}}
[02198]  {:set, {:reg, 0}, {:reg, 1}}
[02201]  {:set, {:reg, 1}, {:reg, 2}}
[02204]  {:set, {:reg, 2}, {:reg, 0}}
[02207]  {:set, {:reg, 0}, {:value, 0}}
[02210]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02214]  {:gt, {:reg, 3}, {:reg, 1}, {:reg, 0}}
[02218]  {:jt, {:reg, 3}, {:value, 2233}}
[02221]  {:add, {:reg, 2}, {:reg, 2}, {:value, 32767}}
[02225]  {:jt, {:reg, 2}, {:value, 2210}}
[02228]  {:set, {:reg, 1}, {:value, 0}}
[02231]  {:jmp, {:value, 2236}}
[02233]  {:set, {:reg, 1}, {:value, 1}}
[02236]  {:pop, {:reg, 3}}
[02238]  {:pop, {:reg, 2}}
[02240]  {:ret}
[02241]  {:set, {:reg, 0}, {:value, 0}}
[02244]  {:set, {:reg, 1}, {:value, 0}}
[02247]  {:ret}
[02248]  {:push, {:reg, 1}}
[02250]  {:push, {:reg, 2}}
[02252]  {:jf, {:reg, 1}, {:value, 2276}}
[02255]  {:add, {:reg, 1}, {:reg, 1}, {:value, 32767}}
[02259]  {:and, {:reg, 2}, {:reg, 0}, {:value, 16384}}
[02263]  {:mult, {:reg, 0}, {:reg, 0}, {:value, 2}}
[02267]  {:jf, {:reg, 2}, {:value, 2252}}
[02270]  {:or, {:reg, 0}, {:reg, 0}, {:value, 1}}
[02274]  {:jmp, {:value, 2252}}
[02276]  {:pop, {:reg, 2}}
[02278]  {:pop, {:reg, 1}}
[02280]  {:ret}
[02281]  {:push, {:reg, 1}}
[02283]  {:gt, {:reg, 1}, {:reg, 0}, {:value, 14}}
[02287]  {:jt, {:reg, 1}, {:value, 2309}}
[02290]  {:set, {:reg, 1}, {:reg, 0}}
[02293]  {:set, {:reg, 0}, {:value, 1}}
[02296]  {:jf, {:reg, 1}, {:value, 2312}}
[02299]  {:add, {:reg, 1}, {:reg, 1}, {:value, 32767}}
[02303]  {:mult, {:reg, 0}, {:reg, 0}, {:value, 2}}
[02307]  {:jmp, {:value, 2296}}
[02309]  {:set, {:reg, 0}, {:value, 32767}}
[02312]  {:pop, {:reg, 1}}
[02314]  {:ret}
[02315]  {:jmp, {:value, 2734}}
[02317]  {:unknown, [6142]}				# Room description pointers
[02318]  {:unknown, [6152]}
[02319]  {:unknown, [26900]}
[02320]  {:unknown, [26903]}
[02321]  {:halt}
[02322]  {:unknown, [6350]}
[02323]  {:unknown, [6360]}
[02324]  {:unknown, [26906]}
[02325]  {:unknown, [26908]}
[02326]  {:halt}
[02327]  {:unknown, [6441]}
[02328]  {:unknown, [6451]}
[02329]  {:unknown, [26910]}
[02330]  {:unknown, [26913]}
[02331]  {:halt}
[02332]  {:unknown, [6597]}
[02333]  {:unknown, [6607]}
[02334]  {:unknown, [26916]}
[02335]  {:unknown, [26919]}
[02336]  {:halt}
[02337]  {:unknown, [6711]}
[02338]  {:unknown, [6721]}
[02339]  {:unknown, [26922]}
[02340]  {:unknown, [26925]}
[02341]  {:halt}
[02342]  {:unknown, [6922]}
[02343]  {:unknown, [6934]}
[02344]  {:unknown, [26928]}
[02345]  {:unknown, [26931]}
[02346]  {:halt}
[02347]  {:unknown, [7070]}
[02348]  {:unknown, [7095]}
[02349]  {:unknown, [26934]}
[02350]  {:unknown, [26936]}
[02351]  {:halt}
[02352]  {:unknown, [7270]}
[02353]  {:unknown, [7282]}
[02354]  {:unknown, [26938]}
[02355]  {:unknown, [26941]}
[02356]  {:halt}
[02357]  {:unknown, [7502]}
[02358]  {:unknown, [7514]}
[02359]  {:unknown, [26944]}
[02360]  {:unknown, [26946]}
[02361]  {:halt}
[02362]  {:unknown, [7616]}
[02363]  {:unknown, [7628]}
[02364]  {:unknown, [26948]}
[02365]  {:unknown, [26951]}
[02366]  {:halt}
[02367]  {:unknown, [7799]}
[02368]  {:unknown, [7807]}
[02369]  {:unknown, [26954]}
[02370]  {:unknown, [26958]}
[02371]  {:halt}
[02372]  {:unknown, [8007]}
[02373]  {:unknown, [26962]}
[02374]  {:unknown, [26965]}
[02375]  {:unknown, [26968]}
[02376]  {:halt}
[02377]  {:unknown, [8163]}
[02378]  {:unknown, [8179]}
[02379]  {:unknown, [26971]}
[02380]  {:unknown, [26977]}
[02381]  {:unknown, [3742]}
[02382]  {:unknown, [8332]}
[02383]  {:unknown, [8348]}
[02384]  {:unknown, [26983]}
[02385]  {:unknown, [26987]}
[02386]  {:unknown, [3746]}
[02387]  {:unknown, [8421]}
[02388]  {:unknown, [8437]}
[02389]  {:unknown, [26991]}
[02390]  {:unknown, [26995]}
[02391]  {:unknown, [3756]}
[02392]  {:unknown, [8510]}
[02393]  {:unknown, [8526]}
[02394]  {:unknown, [26999]}
[02395]  {:unknown, [27004]}
[02396]  {:unknown, [3766]}
[02397]  {:unknown, [8673]}
[02398]  {:unknown, [8689]}
[02399]  {:unknown, [27009]}
[02400]  {:unknown, [27013]}
[02401]  {:unknown, [3776]}
[02402]  {:unknown, [8762]}
[02403]  {:unknown, [8778]}
[02404]  {:unknown, [27017]}
[02405]  {:unknown, [27022]}
[02406]  {:unknown, [3786]}
[02407]  {:unknown, [8932]}
[02408]  {:unknown, [8948]}
[02409]  {:unknown, [27027]}
[02410]  {:unknown, [27031]}
[02411]  {:unknown, [3796]}
[02412]  {:unknown, [9021]}
[02413]  {:unknown, [9037]}
[02414]  {:unknown, [27035]}
[02415]  {:unknown, [27037]}
[02416]  {:unknown, [3806]}
[02417]  {:unknown, [9098]}
[02418]  {:unknown, [9114]}
[02419]  {:unknown, [27039]}
[02420]  {:unknown, [27041]}
[02421]  {:halt}
[02422]  {:unknown, [9175]}
[02423]  {:unknown, [9191]}
[02424]  {:unknown, [27043]}
[02425]  {:unknown, [27046]}
[02426]  {:unknown, [3656]}
[02427]  {:unknown, [9259]}
[02428]  {:unknown, [9272]}
[02429]  {:unknown, [27049]}
[02430]  {:unknown, [27052]}
[02431]  {:unknown, [3656]}
[02432]  {:unknown, [9397]}
[02433]  {:unknown, [9410]}
[02434]  {:unknown, [27055]}
[02435]  {:unknown, [27058]}
[02436]  {:unknown, [3656]}
[02437]  {:unknown, [9455]}
[02438]  {:unknown, [9468]}
[02439]  {:unknown, [27061]}
[02440]  {:unknown, [27064]}
[02441]  {:unknown, [3656]}
[02442]  {:unknown, [9513]}
[02443]  {:unknown, [9526]}
[02444]  {:unknown, [27067]}
[02445]  {:unknown, [27070]}
[02446]  {:unknown, [3656]}
[02447]  {:unknown, [9637]}
[02448]  {:unknown, [9643]}
[02449]  {:unknown, [27073]}
[02450]  {:unknown, [27076]}
[02451]  {:halt}
[02452]  {:unknown, [9903]}
[02453]  {:unknown, [9909]}
[02454]  {:unknown, [27079]}
[02455]  {:unknown, [27082]}
[02456]  {:halt}
[02457]  {:unknown, [10103]}
[02458]  {:unknown, [10109]}
[02459]  {:unknown, [27085]}
[02460]  {:unknown, [27090]}
[02461]  {:halt}
[02462]  {:halt}
[02463]  {:unknown, [10425]}
[02464]  {:unknown, [10431]}
[02465]  {:unknown, [27107]}
[02466]  {:unknown, [27109]}
[02467]  {:unknown, [3893]}
[02468]  {:unknown, [10605]}
[02469]  {:unknown, [10611]}
[02470]  {:unknown, [27111]}
[02471]  {:unknown, [27114]}
[02472]  {:halt}
[02473]  {:unknown, [10760]}
[02474]  {:unknown, [10766]}
[02475]  {:unknown, [27117]}
[02476]  {:unknown, [27119]}
[02477]  {:halt}
[02478]  {:unknown, [10892]}
[02479]  {:unknown, [10898]}
[02480]  {:unknown, [27121]}
[02481]  {:unknown, [27124]}
[02482]  {:halt}
[02483]  {:unknown, [11086]}
[02484]  {:unknown, [11092]}
[02485]  {:unknown, [27127]}
[02486]  {:unknown, [27129]}
[02487]  {:halt}
[02488]  {:unknown, [11267]}
[02489]  {:unknown, [11288]}
[02490]  {:unknown, [27131]}
[02491]  {:unknown, [27133]}
[02492]  {:halt}
[02493]  {:unknown, [11552]}
[02494]  {:unknown, [11573]}
[02495]  {:unknown, [27135]}
[02496]  {:unknown, [27137]}
[02497]  {:halt}
[02498]  {:unknown, [11726]}
[02499]  {:unknown, [11732]}
[02500]  {:unknown, [27139]}
[02501]  {:unknown, [27143]}
[02502]  {:halt}
[02503]  {:unknown, [11919]}
[02504]  {:unknown, [11925]}
[02505]  {:unknown, [27147]}
[02506]  {:unknown, [27150]}
[02507]  {:halt}
[02508]  {:unknown, [12199]}
[02509]  {:unknown, [12205]}
[02510]  {:unknown, [27153]}
[02511]  {:unknown, [27156]}
[02512]  {:halt}
[02513]  {:unknown, [12459]}
[02514]  {:unknown, [12475]}
[02515]  {:unknown, [27159]}
[02516]  {:unknown, [27163]}
[02517]  {:halt}
[02518]  {:unknown, [12691]}
[02519]  {:unknown, [12707]}
[02520]  {:unknown, [27167]}
[02521]  {:unknown, [27171]}
[02522]  {:halt}
[02523]  {:unknown, [12847]}
[02524]  {:unknown, [12863]}
[02525]  {:unknown, [27175]}
[02526]  {:unknown, [27178]}
[02527]  {:halt}
[02528]  {:unknown, [13106]}
[02529]  {:unknown, [13122]}
[02530]  {:unknown, [27181]}
[02531]  {:unknown, [27184]}
[02532]  {:halt}
[02533]  {:unknown, [13420]}
[02534]  {:unknown, [13436]}
[02535]  {:unknown, [27187]}
[02536]  {:unknown, [27190]}
[02537]  {:halt}
[02538]  {:unknown, [13694]}
[02539]  {:unknown, [13708]}
[02540]  {:unknown, [27193]}
[02541]  {:unknown, [27196]}
[02542]  {:halt}
[02543]  {:unknown, [13941]}
[02544]  {:unknown, [13955]}
[02545]  {:unknown, [27199]}
[02546]  {:unknown, [27202]}
[02547]  {:halt}
[02548]  {:unknown, [14090]}
[02549]  {:unknown, [14104]}
[02550]  {:unknown, [27205]}
[02551]  {:unknown, [27209]}
[02552]  {:halt}
[02553]  {:unknown, [14444]}
[02554]  {:unknown, [14465]}
[02555]  {:unknown, [27213]}
[02556]  {:unknown, [27215]}
[02557]  {:halt}
[02558]  {:unknown, [14634]}
[02559]  {:unknown, [14648]}
[02560]  {:unknown, [27217]}
[02561]  {:unknown, [27220]}
[02562]  {:unknown, [4611]}
[02563]  {:unknown, [14765]}
[02564]  {:unknown, [14776]}
[02565]  {:unknown, [27223]}
[02566]  {:unknown, [27226]}
[02567]  {:unknown, [3958]}
[02568]  {:unknown, [14916]}
[02569]  {:unknown, [14927]}
[02570]  {:unknown, [27229]}
[02571]  {:unknown, [27233]}
[02572]  {:unknown, [3975]}
[02573]  {:unknown, [15074]}
[02574]  {:unknown, [15085]}
[02575]  {:unknown, [27237]}
[02576]  {:unknown, [27241]}
[02577]  {:unknown, [3992]}
[02578]  {:unknown, [15230]}
[02579]  {:unknown, [15241]}
[02580]  {:unknown, [27245]}
[02581]  {:unknown, [27249]}
[02582]  {:unknown, [4009]}
[02583]  {:unknown, [15505]}
[02584]  {:unknown, [15516]}
[02585]  {:unknown, [27253]}
[02586]  {:unknown, [27257]}
[02587]  {:unknown, [4028]}
[02588]  {:unknown, [15664]}
[02589]  {:unknown, [15675]}
[02590]  {:unknown, [27261]}
[02591]  {:unknown, [27266]}
[02592]  {:unknown, [4045]}
[02593]  {:unknown, [15826]}
[02594]  {:unknown, [15837]}
[02595]  {:unknown, [27271]}
[02596]  {:unknown, [27276]}
[02597]  {:unknown, [4062]}
[02598]  {:unknown, [15991]}
[02599]  {:unknown, [16002]}
[02600]  {:unknown, [27281]}
[02601]  {:unknown, [27285]}
[02602]  {:unknown, [4079]}
[02603]  {:unknown, [16148]}
[02604]  {:unknown, [16159]}
[02605]  {:unknown, [27289]}
[02606]  {:unknown, [27293]}
[02607]  {:unknown, [4096]}
[02608]  {:unknown, [16305]}
[02609]  {:unknown, [16316]}
[02610]  {:unknown, [27297]}
[02611]  {:unknown, [27302]}
[02612]  {:unknown, [4113]}
[02613]  {:unknown, [16469]}
[02614]  {:unknown, [16480]}
[02615]  {:unknown, [27307]}
[02616]  {:unknown, [27312]}
[02617]  {:unknown, [4130]}
[02618]  {:unknown, [16631]}
[02619]  {:unknown, [16642]}
[02620]  {:unknown, [27317]}
[02621]  {:unknown, [27321]}
[02622]  {:unknown, [4147]}
[02623]  {:unknown, [16791]}
[02624]  {:unknown, [16809]}
[02625]  {:unknown, [27325]}
[02626]  {:unknown, [27329]}
[02627]  {:unknown, [4164]}
[02628]  {:unknown, [16971]}
[02629]  {:unknown, [16982]}
[02630]  {:unknown, [27333]}
[02631]  {:unknown, [27337]}
[02632]  {:unknown, [4167]}
[02633]  {:unknown, [17127]}
[02634]  {:unknown, [17138]}
[02635]  {:unknown, [27341]}
[02636]  {:unknown, [27345]}
[02637]  {:unknown, [4184]}
[02638]  {:unknown, [17285]}
[02639]  {:unknown, [17296]}
[02640]  {:unknown, [27349]}
[02641]  {:unknown, [27352]}
[02642]  {:unknown, [4201]}
[02643]  {:unknown, [17436]}
[02644]  {:unknown, [17442]}
[02645]  {:unknown, [27355]}
[02646]  {:unknown, [27357]}
[02647]  {:unknown, [4690]}
[02648]  {:unknown, [17661]}
[02649]  {:unknown, [17693]}
[02650]  {:unknown, [27359]}
[02651]  {:unknown, [27362]}
[02652]  {:halt}
[02653]  {:unknown, [17806]}
[02654]  {:unknown, [17838]}
[02655]  {:unknown, [27365]}
[02656]  {:unknown, [27368]}
[02657]  {:halt}
[02658]  {:unknown, [17949]}
[02659]  {:unknown, [17967]}
[02660]  {:unknown, [27371]}
[02661]  {:unknown, [27375]}
[02662]  {:halt}
[02663]  {:unknown, [18055]}
[02664]  {:unknown, [18061]}
[02665]  {:unknown, [27379]}
[02666]  {:unknown, [27380]}
[02667]  {:unknown, [3701]}
[02668]  {:unknown, [18062]}
[02669]  {:unknown, [18069]}
[02670]  {:halt}
[02671]  {:unknown, [4720]}
[02672]  {:unknown, [18206]}
[02673]  {:unknown, [18220]}
[02674]  {:unknown, [2357]}
[02675]  {:halt}
[02676]  {:unknown, [18344]}
[02677]  {:unknown, [18352]}
[02678]  {:unknown, [32767]}
[02679]  {:unknown, [4885]}
[02680]  {:unknown, [18468]}
[02681]  {:unknown, [18480]}
[02682]  {:halt}
[02683]  {:unknown, [4931]}
[02684]  {:unknown, [18568]}
[02685]  {:unknown, [18572]}
[02686]  {:unknown, [2417]}
[02687]  {:unknown, [4799]}
[02688]  {:unknown, [18618]}
[02689]  {:unknown, [18627]}
[02690]  {:halt}
[02691]  {:unknown, [5360]}
[02692]  {:unknown, [18691]}
[02693]  {:unknown, [18705]}
[02694]  {:halt}
[02695]  {:unknown, [5377]}
[02696]  {:unknown, [18769]}
[02697]  {:unknown, [18780]}
[02698]  {:halt}
[02699]  {:unknown, [5394]}
[02700]  {:unknown, [18852]}
[02701]  {:unknown, [18865]}
[02702]  {:halt}
[02703]  {:unknown, [5411]}
[02704]  {:unknown, [18953]}
[02705]  {:unknown, [18963]}
[02706]  {:halt}
[02707]  {:unknown, [5428]}
[02708]  {:unknown, [19029]}
[02709]  {:unknown, [19040]}
[02710]  {:unknown, [32767]}
[02711]  {:unknown, [5445]}
[02712]  {:unknown, [19113]}
[02713]  {:unknown, [19127]}
[02714]  {:halt}
[02715]  {:halt}
[02716]  {:unknown, [19192]}
[02717]  {:unknown, [19196]}
[02718]  {:unknown, [2623]}
[02719]  {:halt}
[02720]  {:unknown, [19258]}
[02721]  {:unknown, [19265]}
[02722]  {:unknown, [2643]}
[02723]  {:unknown, [5721]}
[02724]  {:unknown, [19384]}
[02725]  {:unknown, [19397]}
[02726]  {:halt}
[02727]  {:halt}
[02728]  {:unknown, [22093]}
[02729]  {:unknown, [22101]}
[02730]  {:unknown, [2553]}
[02731]  {:halt}
[02732]  {:unknown, [2498]}				# Current room pointer
[02733]  {:unknown, [2498]}
[02734]  {:push, {:reg, 0}}				# Main loop
[02736]  {:push, {:reg, 1}}
[02738]  {:push, {:reg, 2}}
[02740]  {:push, {:reg, 3}}
[02742]  {:rmem, {:reg, 1}, {:value, 2732}}
[02745]  {:rmem, {:reg, 0}, {:value, 2733}}
[02748]  {:eq, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02752]  {:jt, {:reg, 0}, {:value, 2770}}
[02755]  {:rmem, {:reg, 0}, {:value, 2732}}
[02758]  {:add, {:reg, 0}, {:reg, 0}, {:value, 4}}
[02762]  {:rmem, {:reg, 0}, {:reg, 0}}
[02765]  {:jf, {:reg, 0}, {:value, 2770}}
[02768]  {:call, {:reg, 0}}
[02770]  {:rmem, {:reg, 1}, {:value, 2732}}
[02773]  {:rmem, {:reg, 0}, {:value, 2733}}
[02776]  {:eq, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[02780]  {:jt, {:reg, 0}, {:value, 2791}}
[02783]  {:set, {:reg, 0}, {:value, 25974}}
[02786]  {:wmem, {:reg, 0}, {:value, 0}}
[02789]  {:call, {:value, 2964}}				# Call print room description
[02791]  {:wmem, {:value, 2733}, {:reg, 1}}				# Main loop - main cycle point
[02794]  {:push, {:reg, 0}}
[02796]  {:push, {:reg, 1}}
[02798]  {:push, {:reg, 2}}
[02800]  {:set, {:reg, 0}, {:value, 27414}}
[02803]  {:set, {:reg, 1}, {:value, 1531}}
[02806]  {:add, {:reg, 2}, {:value, 7734}, {:value, 9395}}
[02810]  {:call, {:value, 1458}}				# Write memory at r0 to output
[02812]  {:pop, {:reg, 2}}
[02814]  {:pop, {:reg, 1}}
[02816]  {:pop, {:reg, 0}}
[02818]  {:set, {:reg, 0}, {:value, 32}}
[02821]  {:set, {:reg, 1}, {:value, 25974}}
[02824]  {:call, {:value, 1767}}				# Get input from user
[02826]  {:out_newline, {:value, :newline}}
[02828]  {:out_newline, {:value, :newline}}
[02830]  {:set, {:reg, 0}, {:value, 25974}}
[02833]  {:set, {:reg, 1}, {:value, 32}}
[02836]  {:call, {:value, 1571}}
[02838]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32767}}
[02842]  {:jf, {:reg, 1}, {:value, 2848}}
[02845]  {:rmem, {:reg, 0}, {:value, 25974}}
[02848]  {:set, {:reg, 2}, {:reg, 0}}
[02851]  {:rmem, {:reg, 1}, {:value, 25974}}
[02854]  {:push, {:reg, 1}}
[02856]  {:wmem, {:value, 25974}, {:reg, 2}}
[02859]  {:set, {:reg, 0}, {:value, 27398}}
[02862]  {:set, {:reg, 1}, {:value, 25974}}
[02865]  {:call, {:value, 1588}}
[02867]  {:pop, {:reg, 1}}
[02869]  {:wmem, {:value, 25974}, {:reg, 1}}
[02872]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32767}}
[02876]  {:jf, {:reg, 1}, {:value, 2885}}
[02879]  {:set, {:reg, 0}, {:value, 0}}
[02882]  {:set, {:reg, 2}, {:value, 0}}
[02885]  {:add, {:reg, 1}, {:value, 27406}, {:value, 1}}
[02889]  {:add, {:reg, 1}, {:reg, 1}, {:reg, 0}}
[02893]  {:rmem, {:reg, 1}, {:reg, 1}}
[02896]  {:rmem, {:reg, 3}, {:value, 25974}}
[02899]  {:eq, {:reg, 3}, {:reg, 3}, {:reg, 2}}
[02903]  {:jt, {:reg, 3}, {:value, 2944}}
[02906]  {:mult, {:reg, 0}, {:reg, 2}, {:value, 32767}}
[02910]  {:rmem, {:reg, 3}, {:value, 25974}}
[02913]  {:add, {:reg, 3}, {:reg, 0}, {:reg, 3}}
[02917]  {:jf, {:reg, 2}, {:value, 2924}}
[02920]  {:add, {:reg, 3}, {:reg, 3}, {:value, 32767}}
[02924]  {:mod, {:reg, 3}, {:reg, 3}, {:value, 32}}
[02928]  {:add, {:reg, 0}, {:value, 25974}, {:reg, 2}}
[02932]  {:jf, {:reg, 2}, {:value, 2939}}
[02935]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[02939]  {:wmem, {:reg, 0}, {:reg, 3}}
[02942]  {:jmp, {:value, 2950}}
[02944]  {:set, {:reg, 0}, {:value, 25974}}
[02947]  {:wmem, {:reg, 0}, {:value, 0}}
[02950]  {:call, {:reg, 1}}				# React to user input
[02952]  {:jt, {:reg, 1}, {:value, 2742}}
[02955]  {:pop, {:reg, 3}}
[02957]  {:pop, {:reg, 2}}
[02959]  {:pop, {:reg, 1}}
[02961]  {:pop, {:reg, 0}}
[02963]  {:ret}
[02964]  {:push, {:reg, 0}}				# Print room description
[02966]  {:push, {:reg, 1}}
[02968]  {:push, {:reg, 2}}
[02970]  {:rmem, {:reg, 1}, {:reg, 0}}
[02973]  {:jf, {:reg, 1}, {:value, 3032}}
[02976]  {:call, {:value, 5921}}
[02978]  {:jf, {:reg, 0}, {:value, 3006}}
[02981]  {:push, {:reg, 0}}
[02983]  {:call, {:value, 5990}}
[02985]  {:set, {:reg, 1}, {:reg, 0}}
[02988]  {:pop, {:reg, 0}}
[02990]  {:jf, {:reg, 1}, {:value, 3006}}
[02993]  {:add, {:reg, 1}, {:reg, 0}, {:value, 1}}
[02997]  {:rmem, {:reg, 0}, {:reg, 1}}
[03000]  {:call, {:value, 1518}}
[03002]  {:out_newline, {:value, :newline}}
[03004]  {:jmp, {:value, 3238}}
[03006]  {:push, {:reg, 0}}
[03008]  {:push, {:reg, 1}}
[03010]  {:push, {:reg, 2}}
[03012]  {:set, {:reg, 0}, {:value, 27432}}
[03015]  {:set, {:reg, 1}, {:value, 1531}}
[03018]  {:add, {:reg, 2}, {:value, 4595}, {:value, 2160}}
[03022]  {:call, {:value, 1458}}				# Read room title
[03024]  {:pop, {:reg, 2}}
[03026]  {:pop, {:reg, 1}}
[03028]  {:pop, {:reg, 0}}
[03030]  {:jmp, {:value, 3238}}
[03032]  {:rmem, {:reg, 0}, {:value, 2732}}
[03035]  {:push, {:reg, 0}}
[03037]  {:out, {:value, '== '}}
[03043]  {:add, {:reg, 0}, {:reg, 0}, {:value, 0}}
[03047]  {:rmem, {:reg, 0}, {:reg, 0}}
[03050]  {:call, {:value, 1518}}				# Print room title
[03052]  {:out, {:value, ' =='}}
[03058]  {:out_newline, {:value, :newline}}
[03060]  {:pop, {:reg, 0}}
[03062]  {:push, {:reg, 0}}
[03064]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[03068]  {:rmem, {:reg, 0}, {:reg, 0}}
[03071]  {:rmem, {:reg, 1}, {:reg, 0}}
[03074]  {:eq, {:reg, 1}, {:reg, 1}, {:value, 2}}
[03078]  {:jf, {:reg, 1}, {:value, 3104}}
[03081]  {:push, {:reg, 0}}
[03083]  {:set, {:reg, 0}, {:value, 2680}}
[03086]  {:call, {:value, 5990}}
[03088]  {:set, {:reg, 1}, {:reg, 0}}
[03091]  {:pop, {:reg, 0}}
[03093]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[03097]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[03101]  {:rmem, {:reg, 0}, {:reg, 0}}
[03104]  {:call, {:value, 1518}}				# Print room description
[03106]  {:out_newline, {:value, :newline}}
[03108]  {:pop, {:reg, 0}}
[03110]  {:push, {:reg, 0}}
[03112]  {:call, {:value, 5823}}
[03114]  {:jf, {:reg, 0}, {:value, 3146}}
[03117]  {:push, {:reg, 0}}
[03119]  {:push, {:reg, 1}}
[03121]  {:push, {:reg, 2}}
[03123]  {:set, {:reg, 0}, {:value, 27455}}
[03126]  {:set, {:reg, 1}, {:value, 1531}}
[03129]  {:add, {:reg, 2}, {:value, 23069}, {:value, 8535}}
[03133]  {:call, {:value, 1458}}				# Print "things of interest" header if there are interesting things
[03135]  {:pop, {:reg, 2}}
[03137]  {:pop, {:reg, 1}}
[03139]  {:pop, {:reg, 0}}
[03141]  {:rmem, {:reg, 2}, {:value, 2732}}
[03144]  {:call, {:value, 5876}}				# Print things of interest
[03146]  {:pop, {:reg, 0}}
[03148]  {:push, {:reg, 0}}
[03150]  {:add, {:reg, 0}, {:reg, 0}, {:value, 2}}
[03154]  {:rmem, {:reg, 0}, {:reg, 0}}
[03157]  {:rmem, {:reg, 0}, {:reg, 0}}
[03160]  {:eq, {:reg, 2}, {:reg, 0}, {:value, 1}}
[03164]  {:out_newline, {:value, :newline}}
[03166]  {:out, {:value, 'There '}}
[03178]  {:jt, {:reg, 2}, {:value, 3189}}
[03181]  {:out, {:value, 'are'}}
[03187]  {:jmp, {:value, 3193}}
[03189]  {:out, {:value, 'is '}}
[03195]  {:call, {:value, 2001}}
[03197]  {:out, {:value, ' exit'}}
[03207]  {:eq, {:reg, 2}, {:reg, 0}, {:value, 1}}
[03211]  {:jt, {:reg, 2}, {:value, 3216}}
[03214]  {:out, {:value, 's:'}}
[03218]  {:out_newline, {:value, :newline}}
[03220]  {:pop, {:reg, 0}}
[03222]  {:push, {:reg, 0}}
[03224]  {:add, {:reg, 0}, {:reg, 0}, {:value, 2}}
[03228]  {:rmem, {:reg, 0}, {:reg, 0}}
[03231]  {:set, {:reg, 1}, {:value, 5814}}
[03234]  {:call, {:value, 1458}}				# Print exits
[03236]  {:pop, {:reg, 0}}
[03238]  {:pop, {:reg, 2}}
[03240]  {:pop, {:reg, 1}}
[03242]  {:pop, {:reg, 0}}
[03244]  {:ret}
[03245]  {:push, {:reg, 0}}
[03247]  {:push, {:reg, 1}}
[03249]  {:push, {:reg, 2}}
[03251]  {:set, {:reg, 1}, {:reg, 0}}
[03254]  {:rmem, {:reg, 0}, {:value, 2732}}
[03257]  {:add, {:reg, 0}, {:reg, 0}, {:value, 2}}
[03261]  {:rmem, {:reg, 0}, {:reg, 0}}
[03264]  {:call, {:value, 1588}}
[03266]  {:eq, {:reg, 2}, {:reg, 0}, {:value, 32767}}
[03270]  {:jt, {:reg, 2}, {:value, 3302}}
[03273]  {:rmem, {:reg, 2}, {:value, 2732}}
[03276]  {:add, {:reg, 2}, {:reg, 2}, {:value, 3}}
[03280]  {:rmem, {:reg, 2}, {:reg, 2}}
[03283]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[03287]  {:add, {:reg, 2}, {:reg, 2}, {:reg, 0}}
[03291]  {:rmem, {:reg, 2}, {:reg, 2}}
[03294]  {:wmem, {:value, 2732}, {:reg, 2}}
[03297]  {:wmem, {:value, 2733}, {:value, 0}}
[03300]  {:jmp, {:value, 3326}}
[03302]  {:push, {:reg, 0}}
[03304]  {:push, {:reg, 1}}
[03306]  {:push, {:reg, 2}}
[03308]  {:set, {:reg, 0}, {:value, 27482}}
[03311]  {:set, {:reg, 1}, {:value, 1531}}
[03314]  {:add, {:reg, 2}, {:value, 8104}, {:value, 8912}}
[03318]  {:call, {:value, 1458}}
[03320]  {:pop, {:reg, 2}}
[03322]  {:pop, {:reg, 1}}
[03324]  {:pop, {:reg, 0}}
[03326]  {:pop, {:reg, 2}}
[03328]  {:pop, {:reg, 1}}
[03330]  {:pop, {:reg, 0}}
[03332]  {:ret}
[03333]  {:push, {:reg, 0}}
[03335]  {:push, {:reg, 0}}
[03337]  {:push, {:reg, 1}}
[03339]  {:push, {:reg, 2}}
[03341]  {:set, {:reg, 0}, {:value, 27532}}
[03344]  {:set, {:reg, 1}, {:value, 1531}}
[03347]  {:add, {:reg, 2}, {:value, 4592}, {:value, 25603}}
[03351]  {:call, {:value, 1458}}
[03353]  {:pop, {:reg, 2}}
[03355]  {:pop, {:reg, 1}}
[03357]  {:pop, {:reg, 0}}
[03359]  {:pop, {:reg, 0}}
[03361]  {:ret}
[03362]  {:push, {:reg, 0}}
[03364]  {:push, {:reg, 2}}
[03366]  {:push, {:reg, 0}}
[03368]  {:push, {:reg, 1}}
[03370]  {:push, {:reg, 2}}
[03372]  {:set, {:reg, 0}, {:value, 28037}}
[03375]  {:set, {:reg, 1}, {:value, 1531}}
[03378]  {:add, {:reg, 2}, {:value, 432}, {:value, 1484}}
[03382]  {:call, {:value, 1458}}
[03384]  {:pop, {:reg, 2}}
[03386]  {:pop, {:reg, 1}}
[03388]  {:pop, {:reg, 0}}
[03390]  {:set, {:reg, 2}, {:value, 0}}
[03393]  {:call, {:value, 5876}}
[03395]  {:pop, {:reg, 2}}
[03397]  {:pop, {:reg, 0}}
[03399]  {:ret}
[03400]  {:push, {:reg, 0}}
[03402]  {:push, {:reg, 1}}
[03404]  {:push, {:reg, 2}}
[03406]  {:call, {:value, 5921}}
[03408]  {:jf, {:reg, 0}, {:value, 3457}}
[03411]  {:add, {:reg, 1}, {:reg, 0}, {:value, 2}}
[03415]  {:rmem, {:reg, 0}, {:reg, 1}}
[03418]  {:rmem, {:reg, 2}, {:value, 2732}}
[03421]  {:eq, {:reg, 2}, {:reg, 0}, {:reg, 2}}
[03425]  {:jf, {:reg, 2}, {:value, 3457}}
[03428]  {:wmem, {:reg, 1}, {:value, 0}}
[03431]  {:push, {:reg, 0}}
[03433]  {:push, {:reg, 1}}
[03435]  {:push, {:reg, 2}}
[03437]  {:set, {:reg, 0}, {:value, 28054}}
[03440]  {:set, {:reg, 1}, {:value, 1531}}
[03443]  {:add, {:reg, 2}, {:value, 238}, {:value, 5885}}
[03447]  {:call, {:value, 1458}}
[03449]  {:pop, {:reg, 2}}
[03451]  {:pop, {:reg, 1}}
[03453]  {:pop, {:reg, 0}}
[03455]  {:jmp, {:value, 3481}}
[03457]  {:push, {:reg, 0}}
[03459]  {:push, {:reg, 1}}
[03461]  {:push, {:reg, 2}}
[03463]  {:set, {:reg, 0}, {:value, 28062}}
[03466]  {:set, {:reg, 1}, {:value, 1531}}
[03469]  {:add, {:reg, 2}, {:value, 20833}, {:value, 2578}}
[03473]  {:call, {:value, 1458}}
[03475]  {:pop, {:reg, 2}}
[03477]  {:pop, {:reg, 1}}
[03479]  {:pop, {:reg, 0}}
[03481]  {:pop, {:reg, 2}}
[03483]  {:pop, {:reg, 1}}
[03485]  {:pop, {:reg, 0}}
[03487]  {:ret}
[03488]  {:push, {:reg, 0}}
[03490]  {:push, {:reg, 1}}
[03492]  {:call, {:value, 5921}}
[03494]  {:jf, {:reg, 0}, {:value, 3539}}
[03497]  {:add, {:reg, 1}, {:reg, 0}, {:value, 2}}
[03501]  {:rmem, {:reg, 0}, {:reg, 1}}
[03504]  {:jt, {:reg, 0}, {:value, 3539}}
[03507]  {:rmem, {:reg, 0}, {:value, 2732}}
[03510]  {:wmem, {:reg, 1}, {:reg, 0}}
[03513]  {:push, {:reg, 0}}
[03515]  {:push, {:reg, 1}}
[03517]  {:push, {:reg, 2}}
[03519]  {:set, {:reg, 0}, {:value, 28090}}
[03522]  {:set, {:reg, 1}, {:value, 1531}}
[03525]  {:add, {:reg, 2}, {:value, 1769}, {:value, 14212}}
[03529]  {:call, {:value, 1458}}
[03531]  {:pop, {:reg, 2}}
[03533]  {:pop, {:reg, 1}}
[03535]  {:pop, {:reg, 0}}
[03537]  {:jmp, {:value, 3563}}
[03539]  {:push, {:reg, 0}}
[03541]  {:push, {:reg, 1}}
[03543]  {:push, {:reg, 2}}
[03545]  {:set, {:reg, 0}, {:value, 28100}}
[03548]  {:set, {:reg, 1}, {:value, 1531}}
[03551]  {:add, {:reg, 2}, {:value, 27093}, {:value, 5662}}
[03555]  {:call, {:value, 1458}}
[03557]  {:pop, {:reg, 2}}
[03559]  {:pop, {:reg, 1}}
[03561]  {:pop, {:reg, 0}}
[03563]  {:pop, {:reg, 1}}
[03565]  {:pop, {:reg, 0}}
[03567]  {:ret}
[03568]  {:push, {:reg, 0}}
[03570]  {:push, {:reg, 1}}
[03572]  {:call, {:value, 5921}}
[03574]  {:jf, {:reg, 0}, {:value, 3601}}
[03577]  {:add, {:reg, 1}, {:reg, 0}, {:value, 2}}
[03581]  {:rmem, {:reg, 1}, {:reg, 1}}
[03584]  {:jt, {:reg, 1}, {:value, 3601}}
[03587]  {:add, {:reg, 1}, {:reg, 0}, {:value, 3}}
[03591]  {:rmem, {:reg, 1}, {:reg, 1}}
[03594]  {:jf, {:reg, 1}, {:value, 3627}}
[03597]  {:call, {:reg, 1}}
[03599]  {:jmp, {:value, 3651}}
[03601]  {:push, {:reg, 0}}
[03603]  {:push, {:reg, 1}}
[03605]  {:push, {:reg, 2}}
[03607]  {:set, {:reg, 0}, {:value, 28135}}
[03610]  {:set, {:reg, 1}, {:value, 1531}}
[03613]  {:add, {:reg, 2}, {:value, 17703}, {:value, 10555}}
[03617]  {:call, {:value, 1458}}
[03619]  {:pop, {:reg, 2}}
[03621]  {:pop, {:reg, 1}}
[03623]  {:pop, {:reg, 0}}
[03625]  {:jmp, {:value, 3651}}
[03627]  {:push, {:reg, 0}}
[03629]  {:push, {:reg, 1}}
[03631]  {:push, {:reg, 2}}
[03633]  {:set, {:reg, 0}, {:value, 28170}}
[03636]  {:set, {:reg, 1}, {:value, 1531}}
[03639]  {:add, {:reg, 2}, {:value, 4307}, {:value, 2452}}
[03643]  {:call, {:value, 1458}}
[03645]  {:pop, {:reg, 2}}
[03647]  {:pop, {:reg, 1}}
[03649]  {:pop, {:reg, 0}}
[03651]  {:pop, {:reg, 1}}
[03653]  {:pop, {:reg, 0}}
[03655]  {:ret}
[03656]  {:push, {:reg, 0}}
[03658]  {:push, {:reg, 1}}
[03660]  {:push, {:reg, 2}}
[03662]  {:set, {:reg, 0}, {:value, 2680}}
[03665]  {:call, {:value, 5990}}
[03667]  {:jt, {:reg, 0}, {:value, 3694}}
[03670]  {:add, {:reg, 0}, {:value, 2676}, {:value, 2}}
[03674]  {:wmem, {:reg, 0}, {:value, 32767}}
[03677]  {:add, {:reg, 0}, {:value, 2680}, {:value, 2}}
[03681]  {:wmem, {:reg, 0}, {:value, 32767}}
[03684]  {:add, {:reg, 0}, {:value, 2672}, {:value, 2}}
[03688]  {:wmem, {:reg, 0}, {:value, 32767}}
[03691]  {:wmem, {:value, 2732}, {:value, 2648}}
[03694]  {:pop, {:reg, 2}}
[03696]  {:pop, {:reg, 1}}
[03698]  {:pop, {:reg, 0}}
[03700]  {:ret}
[03701]  {:push, {:reg, 0}}
[03703]  {:push, {:reg, 1}}
[03705]  {:push, {:reg, 2}}
[03707]  {:set, {:reg, 0}, {:value, 28204}}
[03710]  {:set, {:reg, 1}, {:value, 1531}}
[03713]  {:add, {:reg, 2}, {:value, 11030}, {:value, 2765}}
[03717]  {:call, {:value, 1458}}
[03719]  {:pop, {:reg, 2}}
[03721]  {:pop, {:reg, 1}}
[03723]  {:pop, {:reg, 0}}
[03725]  {:halt}
[03726]  {:halt}
[03727]  {:push, {:reg, 1}}
[03729]  {:rmem, {:reg, 1}, {:value, 3726}}
[03732]  {:or, {:reg, 1}, {:reg, 1}, {:reg, 0}}
[03736]  {:wmem, {:value, 3726}, {:reg, 1}}
[03739]  {:pop, {:reg, 1}}
[03741]  {:ret}
[03742]  {:wmem, {:value, 3726}, {:value, 0}}
[03745]  {:ret}
[03746]  {:push, {:reg, 0}}
[03748]  {:set, {:reg, 0}, {:value, 1}}
[03751]  {:call, {:value, 3727}}
[03753]  {:pop, {:reg, 0}}
[03755]  {:ret}
[03756]  {:push, {:reg, 0}}
[03758]  {:set, {:reg, 0}, {:value, 2}}
[03761]  {:call, {:value, 3727}}
[03763]  {:pop, {:reg, 0}}
[03765]  {:ret}
[03766]  {:push, {:reg, 0}}
[03768]  {:set, {:reg, 0}, {:value, 4}}
[03771]  {:call, {:value, 3727}}
[03773]  {:pop, {:reg, 0}}
[03775]  {:ret}
[03776]  {:push, {:reg, 0}}
[03778]  {:set, {:reg, 0}, {:value, 8}}
[03781]  {:call, {:value, 3727}}
[03783]  {:pop, {:reg, 0}}
[03785]  {:ret}
[03786]  {:push, {:reg, 0}}
[03788]  {:set, {:reg, 0}, {:value, 16}}
[03791]  {:call, {:value, 3727}}
[03793]  {:pop, {:reg, 0}}
[03795]  {:ret}
[03796]  {:push, {:reg, 0}}
[03798]  {:set, {:reg, 0}, {:value, 32}}
[03801]  {:call, {:value, 3727}}
[03803]  {:pop, {:reg, 0}}
[03805]  {:ret}
[03806]  {:push, {:reg, 0}}
[03808]  {:push, {:reg, 1}}
[03810]  {:push, {:reg, 2}}
[03812]  {:push, {:reg, 3}}
[03814]  {:set, {:reg, 0}, {:value, 64}}
[03817]  {:call, {:value, 3727}}
[03819]  {:push, {:reg, 0}}
[03821]  {:push, {:reg, 1}}
[03823]  {:push, {:reg, 2}}
[03825]  {:set, {:reg, 0}, {:value, 28236}}
[03828]  {:set, {:reg, 1}, {:value, 1531}}
[03831]  {:add, {:reg, 2}, {:value, 2352}, {:value, 5191}}
[03835]  {:call, {:value, 1458}}
[03837]  {:pop, {:reg, 2}}
[03839]  {:pop, {:reg, 1}}
[03841]  {:pop, {:reg, 0}}
[03843]  {:rmem, {:reg, 0}, {:value, 3726}}
[03846]  {:set, {:reg, 1}, {:value, 25866}}
[03849]  {:set, {:reg, 2}, {:value, 32767}}
[03852]  {:set, {:reg, 3}, {:value, 28299}}
[03855]  {:call, {:value, 1841}}
[03857]  {:push, {:reg, 0}}
[03859]  {:push, {:reg, 1}}
[03861]  {:push, {:reg, 2}}
[03863]  {:set, {:reg, 0}, {:value, 28303}}
[03866]  {:set, {:reg, 1}, {:value, 1531}}
[03869]  {:add, {:reg, 2}, {:value, 2891}, {:value, 21801}}
[03873]  {:call, {:value, 1458}}
[03875]  {:pop, {:reg, 2}}
[03877]  {:pop, {:reg, 1}}
[03879]  {:pop, {:reg, 0}}
[03881]  {:wmem, {:value, 2732}, {:value, 2417}}
[03884]  {:pop, {:reg, 3}}
[03886]  {:pop, {:reg, 2}}
[03888]  {:pop, {:reg, 1}}
[03890]  {:pop, {:reg, 0}}
[03892]  {:ret}
[03893]  {:push, {:reg, 0}}
[03895]  {:push, {:reg, 1}}
[03897]  {:rmem, {:reg, 0}, {:value, 2462}}
[03900]  {:rmem, {:reg, 1}, {:value, 27101}}
[03903]  {:eq, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[03907]  {:jt, {:reg, 0}, {:value, 3940}}
[03910]  {:push, {:reg, 0}}
[03912]  {:push, {:reg, 1}}
[03914]  {:push, {:reg, 2}}
[03916]  {:set, {:reg, 0}, {:value, 28347}}
[03919]  {:set, {:reg, 1}, {:value, 1531}}
[03922]  {:add, {:reg, 2}, {:value, 286}, {:value, 338}}
[03926]  {:call, {:value, 1458}}
[03928]  {:pop, {:reg, 2}}
[03930]  {:pop, {:reg, 1}}
[03932]  {:pop, {:reg, 0}}
[03934]  {:wmem, {:value, 2732}, {:value, 2457}}
[03937]  {:wmem, {:value, 2733}, {:value, 2457}}
[03940]  {:pop, {:reg, 1}}
[03942]  {:pop, {:reg, 0}}
[03944]  {:ret}
[03945]  {:unknown, [26007]}
[03946]  {:unknown, [26013]}
[03947]  {:unknown, [26017]}
[03948]  {:unknown, [2149]}
[03949]  {:unknown, [2158]}
[03950]  {:unknown, [2178]}
[03951]  {:halt}
[03952]  {:unknown, [22]}
[03953]  {:halt}
[03954]  {:halt}
[03955]  {:halt}
[03956]  {:halt}
[03957]  {:halt}
[03958]  {:push, {:reg, 0}}
[03960]  {:push, {:reg, 1}}
[03962]  {:set, {:reg, 0}, {:value, 2}}
[03965]  {:set, {:reg, 1}, {:value, 0}}
[03968]  {:call, {:value, 4218}}
[03970]  {:pop, {:reg, 1}}
[03972]  {:pop, {:reg, 0}}
[03974]  {:ret}
[03975]  {:push, {:reg, 0}}
[03977]  {:push, {:reg, 1}}
[03979]  {:set, {:reg, 0}, {:value, 8}}
[03982]  {:set, {:reg, 1}, {:value, 1}}
[03985]  {:call, {:value, 4279}}
[03987]  {:pop, {:reg, 1}}
[03989]  {:pop, {:reg, 0}}
[03991]  {:ret}
[03992]  {:push, {:reg, 0}}
[03994]  {:push, {:reg, 1}}
[03996]  {:set, {:reg, 0}, {:value, 1}}
[03999]  {:set, {:reg, 1}, {:value, 2}}
[04002]  {:call, {:value, 4218}}
[04004]  {:pop, {:reg, 1}}
[04006]  {:pop, {:reg, 0}}
[04008]  {:ret}
[04009]  {:push, {:reg, 0}}
[04011]  {:push, {:reg, 1}}
[04013]  {:set, {:reg, 0}, {:value, 1}}
[04016]  {:set, {:reg, 1}, {:value, 3}}
[04019]  {:call, {:value, 4279}}
[04021]  {:call, {:value, 4533}}
[04023]  {:pop, {:reg, 1}}
[04025]  {:pop, {:reg, 0}}
[04027]  {:ret}
[04028]  {:push, {:reg, 0}}
[04030]  {:push, {:reg, 1}}
[04032]  {:set, {:reg, 0}, {:value, 4}}
[04035]  {:set, {:reg, 1}, {:value, 4}}
[04038]  {:call, {:value, 4279}}
[04040]  {:pop, {:reg, 1}}
[04042]  {:pop, {:reg, 0}}
[04044]  {:ret}
[04045]  {:push, {:reg, 0}}
[04047]  {:push, {:reg, 1}}
[04049]  {:set, {:reg, 0}, {:value, 2}}
[04052]  {:set, {:reg, 1}, {:value, 5}}
[04055]  {:call, {:value, 4218}}
[04057]  {:pop, {:reg, 1}}
[04059]  {:pop, {:reg, 0}}
[04061]  {:ret}
[04062]  {:push, {:reg, 0}}
[04064]  {:push, {:reg, 1}}
[04066]  {:set, {:reg, 0}, {:value, 11}}
[04069]  {:set, {:reg, 1}, {:value, 6}}
[04072]  {:call, {:value, 4279}}
[04074]  {:pop, {:reg, 1}}
[04076]  {:pop, {:reg, 0}}
[04078]  {:ret}
[04079]  {:push, {:reg, 0}}
[04081]  {:push, {:reg, 1}}
[04083]  {:set, {:reg, 0}, {:value, 2}}
[04086]  {:set, {:reg, 1}, {:value, 7}}
[04089]  {:call, {:value, 4218}}
[04091]  {:pop, {:reg, 1}}
[04093]  {:pop, {:reg, 0}}
[04095]  {:ret}
[04096]  {:push, {:reg, 0}}
[04098]  {:push, {:reg, 1}}
[04100]  {:set, {:reg, 0}, {:value, 0}}
[04103]  {:set, {:reg, 1}, {:value, 8}}
[04106]  {:call, {:value, 4218}}
[04108]  {:pop, {:reg, 1}}
[04110]  {:pop, {:reg, 0}}
[04112]  {:ret}
[04113]  {:push, {:reg, 0}}
[04115]  {:push, {:reg, 1}}
[04117]  {:set, {:reg, 0}, {:value, 4}}
[04120]  {:set, {:reg, 1}, {:value, 9}}
[04123]  {:call, {:value, 4279}}
[04125]  {:pop, {:reg, 1}}
[04127]  {:pop, {:reg, 0}}
[04129]  {:ret}
[04130]  {:push, {:reg, 0}}
[04132]  {:push, {:reg, 1}}
[04134]  {:set, {:reg, 0}, {:value, 1}}
[04137]  {:set, {:reg, 1}, {:value, 10}}
[04140]  {:call, {:value, 4218}}
[04142]  {:pop, {:reg, 1}}
[04144]  {:pop, {:reg, 0}}
[04146]  {:ret}
[04147]  {:push, {:reg, 0}}
[04149]  {:push, {:reg, 1}}
[04151]  {:set, {:reg, 0}, {:value, 18}}
[04154]  {:set, {:reg, 1}, {:value, 11}}
[04157]  {:call, {:value, 4279}}
[04159]  {:pop, {:reg, 1}}
[04161]  {:pop, {:reg, 0}}
[04163]  {:ret}
[04164]  {:call, {:value, 4611}}
[04166]  {:ret}
[04167]  {:push, {:reg, 0}}
[04169]  {:push, {:reg, 1}}
[04171]  {:set, {:reg, 0}, {:value, 1}}
[04174]  {:set, {:reg, 1}, {:value, 12}}
[04177]  {:call, {:value, 4218}}
[04179]  {:pop, {:reg, 1}}
[04181]  {:pop, {:reg, 0}}
[04183]  {:ret}
[04184]  {:push, {:reg, 0}}
[04186]  {:push, {:reg, 1}}
[04188]  {:set, {:reg, 0}, {:value, 9}}
[04191]  {:set, {:reg, 1}, {:value, 13}}
[04194]  {:call, {:value, 4279}}
[04196]  {:pop, {:reg, 1}}
[04198]  {:pop, {:reg, 0}}
[04200]  {:ret}
[04201]  {:push, {:reg, 0}}
[04203]  {:push, {:reg, 1}}
[04205]  {:set, {:reg, 0}, {:value, 2}}
[04208]  {:set, {:reg, 1}, {:value, 14}}
[04211]  {:call, {:value, 4218}}
[04213]  {:pop, {:reg, 1}}
[04215]  {:pop, {:reg, 0}}
[04217]  {:ret}
[04218]  {:push, {:reg, 0}}
[04220]  {:push, {:reg, 1}}
[04222]  {:push, {:reg, 2}}
[04224]  {:add, {:reg, 2}, {:value, 2716}, {:value, 2}}
[04228]  {:rmem, {:reg, 2}, {:reg, 2}}
[04231]  {:jt, {:reg, 2}, {:value, 4272}}
[04234]  {:call, {:value, 4405}}
[04236]  {:wmem, {:value, 3951}, {:reg, 0}}
[04239]  {:add, {:reg, 1}, {:reg, 0}, {:value, 3945}}
[04243]  {:rmem, {:reg, 1}, {:reg, 1}}
[04246]  {:set, {:reg, 0}, {:value, 26024}}
[04249]  {:call, {:value, 1518}}
[04251]  {:set, {:reg, 0}, {:reg, 1}}
[04254]  {:call, {:value, 1518}}
[04256]  {:set, {:reg, 0}, {:value, 26088}}
[04259]  {:call, {:value, 1518}}
[04261]  {:set, {:reg, 0}, {:reg, 1}}
[04264]  {:call, {:value, 1518}}
[04266]  {:out, {:value, '.'}}
[04268]  {:out_newline, {:value, :newline}}
[04270]  {:out_newline, {:value, :newline}}
[04272]  {:pop, {:reg, 2}}
[04274]  {:pop, {:reg, 1}}
[04276]  {:pop, {:reg, 0}}
[04278]  {:ret}
[04279]  {:push, {:reg, 0}}
[04281]  {:push, {:reg, 1}}
[04283]  {:push, {:reg, 2}}
[04285]  {:add, {:reg, 2}, {:value, 2716}, {:value, 2}}
[04289]  {:rmem, {:reg, 2}, {:reg, 2}}
[04292]  {:jt, {:reg, 2}, {:value, 4398}}
[04295]  {:call, {:value, 4405}}
[04297]  {:push, {:reg, 0}}
[04299]  {:rmem, {:reg, 0}, {:value, 3951}}
[04302]  {:add, {:reg, 1}, {:reg, 0}, {:value, 3945}}
[04306]  {:rmem, {:reg, 1}, {:reg, 1}}
[04309]  {:set, {:reg, 0}, {:value, 26122}}
[04312]  {:call, {:value, 1518}}
[04314]  {:set, {:reg, 0}, {:reg, 1}}
[04317]  {:call, {:value, 1518}}
[04319]  {:set, {:reg, 0}, {:value, 26170}}
[04322]  {:call, {:value, 1518}}
[04324]  {:pop, {:reg, 0}}
[04326]  {:set, {:reg, 1}, {:reg, 0}}
[04329]  {:rmem, {:reg, 0}, {:value, 3952}}
[04332]  {:rmem, {:reg, 2}, {:value, 3951}}
[04335]  {:add, {:reg, 2}, {:reg, 2}, {:value, 3948}}
[04339]  {:rmem, {:reg, 2}, {:reg, 2}}
[04342]  {:call, {:reg, 2}}
[04344]  {:jt, {:reg, 1}, {:value, 4391}}
[04347]  {:rmem, {:reg, 1}, {:value, 3952}}
[04350]  {:wmem, {:value, 3952}, {:reg, 0}}
[04353]  {:gt, {:reg, 2}, {:reg, 0}, {:reg, 1}}
[04357]  {:jf, {:reg, 2}, {:value, 4369}}
[04360]  {:push, {:reg, 0}}
[04362]  {:set, {:reg, 0}, {:value, 26235}}
[04365]  {:call, {:value, 1518}}
[04367]  {:pop, {:reg, 0}}
[04369]  {:gt, {:reg, 2}, {:reg, 1}, {:reg, 0}}
[04373]  {:jf, {:reg, 2}, {:value, 4385}}
[04376]  {:push, {:reg, 0}}
[04378]  {:set, {:reg, 0}, {:value, 26267}}
[04381]  {:call, {:value, 1518}}
[04383]  {:pop, {:reg, 0}}
[04385]  {:out_newline, {:value, :newline}}
[04387]  {:out_newline, {:value, :newline}}
[04389]  {:jmp, {:value, 4398}}
[04391]  {:call, {:value, 4660}}
[04393]  {:set, {:reg, 0}, {:value, 26299}}
[04396]  {:call, {:value, 1518}}
[04398]  {:pop, {:reg, 2}}
[04400]  {:pop, {:reg, 1}}
[04402]  {:pop, {:reg, 0}}
[04404]  {:ret}
[04405]  {:push, {:reg, 0}}
[04407]  {:push, {:reg, 1}}
[04409]  {:push, {:reg, 2}}
[04411]  {:push, {:reg, 3}}
[04413]  {:push, {:reg, 4}}
[04415]  {:push, {:reg, 5}}
[04417]  {:rmem, {:reg, 5}, {:value, 3953}}
[04420]  {:gt, {:reg, 3}, {:reg, 5}, {:value, 29999}}
[04424]  {:jt, {:reg, 3}, {:value, 4434}}
[04427]  {:add, {:reg, 5}, {:reg, 5}, {:value, 1}}
[04431]  {:wmem, {:value, 3953}, {:reg, 5}}
[04434]  {:set, {:reg, 3}, {:reg, 0}}
[04437]  {:set, {:reg, 4}, {:reg, 1}}
[04440]  {:add, {:reg, 0}, {:reg, 5}, {:value, 2}}
[04444]  {:call, {:value, 2281}}
[04446]  {:rmem, {:reg, 1}, {:value, 3954}}
[04449]  {:or, {:reg, 0}, {:reg, 1}, {:reg, 0}}
[04453]  {:set, {:reg, 1}, {:reg, 4}}
[04456]  {:call, {:value, 2248}}
[04458]  {:wmem, {:value, 3954}, {:reg, 0}}
[04461]  {:set, {:reg, 0}, {:value, 3955}}
[04464]  {:add, {:reg, 1}, {:reg, 5}, {:value, 2}}
[04468]  {:set, {:reg, 2}, {:reg, 4}}
[04471]  {:call, {:value, 4515}}
[04473]  {:set, {:reg, 0}, {:value, 3956}}
[04476]  {:mult, {:reg, 1}, {:reg, 5}, {:reg, 5}}
[04480]  {:mult, {:reg, 2}, {:reg, 4}, {:reg, 4}}
[04484]  {:call, {:value, 4515}}
[04486]  {:set, {:reg, 0}, {:value, 3957}}
[04489]  {:set, {:reg, 1}, {:value, 13}}
[04492]  {:mult, {:reg, 2}, {:reg, 3}, {:value, 9}}
[04496]  {:mult, {:reg, 2}, {:reg, 2}, {:reg, 2}}
[04500]  {:call, {:value, 4515}}
[04502]  {:pop, {:reg, 5}}
[04504]  {:pop, {:reg, 4}}
[04506]  {:pop, {:reg, 3}}
[04508]  {:pop, {:reg, 2}}
[04510]  {:pop, {:reg, 1}}
[04512]  {:pop, {:reg, 0}}
[04514]  {:ret}
[04515]  {:push, {:reg, 0}}
[04517]  {:rmem, {:reg, 0}, {:reg, 0}}
[04520]  {:call, {:value, 2248}}
[04522]  {:set, {:reg, 1}, {:reg, 2}}
[04525]  {:call, {:value, 2125}}
[04527]  {:pop, {:reg, 1}}
[04529]  {:wmem, {:reg, 1}, {:reg, 0}}
[04532]  {:ret}
[04533]  {:push, {:reg, 0}}
[04535]  {:add, {:reg, 0}, {:value, 2716}, {:value, 2}}
[04539]  {:rmem, {:reg, 0}, {:reg, 0}}
[04542]  {:jt, {:reg, 0}, {:value, 4608}}
[04545]  {:set, {:reg, 0}, {:value, 26321}}
[04548]  {:call, {:value, 1518}}
[04550]  {:rmem, {:reg, 0}, {:value, 3952}}
[04553]  {:eq, {:reg, 0}, {:reg, 0}, {:value, 30}}
[04557]  {:jt, {:reg, 0}, {:value, 4574}}
[04560]  {:set, {:reg, 0}, {:value, 26354}}
[04563]  {:call, {:value, 1518}}
[04565]  {:set, {:reg, 0}, {:value, 26398}}
[04568]  {:call, {:value, 1518}}
[04570]  {:call, {:value, 4660}}
[04572]  {:jmp, {:value, 4608}}
[04574]  {:set, {:reg, 0}, {:value, 26440}}
[04577]  {:call, {:value, 1518}}
[04579]  {:rmem, {:reg, 0}, {:value, 3954}}
[04582]  {:add, {:reg, 0}, {:reg, 0}, {:value, 1}}
[04586]  {:jt, {:reg, 0}, {:value, 4596}}
[04589]  {:set, {:reg, 0}, {:value, 26484}}
[04592]  {:call, {:value, 1518}}
[04594]  {:jmp, {:value, 4565}}
[04596]  {:set, {:reg, 0}, {:value, 26584}}
[04599]  {:call, {:value, 1518}}
[04601]  {:add, {:reg, 0}, {:value, 2716}, {:value, 2}}
[04605]  {:wmem, {:reg, 0}, {:value, 32767}}
[04608]  {:pop, {:reg, 0}}
[04610]  {:ret}
[04611]  {:push, {:reg, 0}}
[04613]  {:add, {:reg, 0}, {:value, 2716}, {:value, 2}}
[04617]  {:rmem, {:reg, 0}, {:reg, 0}}
[04620]  {:jt, {:reg, 0}, {:value, 4657}}
[04623]  {:set, {:reg, 0}, {:value, 26717}}
[04626]  {:call, {:value, 1518}}
[04628]  {:rmem, {:reg, 0}, {:value, 2732}}
[04631]  {:eq, {:reg, 0}, {:reg, 0}, {:value, 2623}}
[04635]  {:jt, {:reg, 0}, {:value, 4645}}
[04638]  {:set, {:reg, 0}, {:value, 26725}}
[04641]  {:call, {:value, 1518}}
[04643]  {:jmp, {:value, 4650}}
[04645]  {:set, {:reg, 0}, {:value, 26731}}
[04648]  {:call, {:value, 1518}}
[04650]  {:set, {:reg, 0}, {:value, 26737}}
[04653]  {:call, {:value, 1518}}
[04655]  {:call, {:value, 4660}}
[04657]  {:pop, {:reg, 0}}
[04659]  {:ret}
[04660]  {:push, {:reg, 0}}
[04662]  {:wmem, {:value, 3952}, {:value, 22}}
[04665]  {:wmem, {:value, 3953}, {:value, 0}}
[04668]  {:wmem, {:value, 3954}, {:value, 0}}
[04671]  {:wmem, {:value, 3955}, {:value, 0}}
[04674]  {:wmem, {:value, 3956}, {:value, 0}}
[04677]  {:wmem, {:value, 3957}, {:value, 0}}
[04680]  {:add, {:reg, 0}, {:value, 2716}, {:value, 2}}
[04684]  {:wmem, {:reg, 0}, {:value, 2623}}
[04687]  {:pop, {:reg, 0}}
[04689]  {:ret}
[04690]  {:push, {:reg, 0}}
[04692]  {:add, {:reg, 0}, {:value, 2716}, {:value, 2}}
[04696]  {:rmem, {:reg, 0}, {:reg, 0}}
[04699]  {:eq, {:reg, 0}, {:reg, 0}, {:value, 32767}}
[04703]  {:jt, {:reg, 0}, {:value, 4717}}
[04706]  {:set, {:reg, 0}, {:value, 26824}}
[04709]  {:call, {:value, 1518}}
[04711]  {:wmem, {:value, 2732}, {:value, 2578}}
[04714]  {:wmem, {:value, 2733}, {:value, 2578}}
[04717]  {:pop, {:reg, 0}}
[04719]  {:ret}
[04720]  {:push, {:reg, 0}}
[04722]  {:push, {:reg, 1}}
[04724]  {:push, {:reg, 2}}
[04726]  {:push, {:reg, 3}}
[04728]  {:push, {:reg, 0}}
[04730]  {:push, {:reg, 1}}
[04732]  {:push, {:reg, 2}}
[04734]  {:set, {:reg, 0}, {:value, 28369}}
[04737]  {:set, {:reg, 1}, {:value, 1531}}
[04740]  {:add, {:reg, 2}, {:value, 10605}, {:value, 12156}}
[04744]  {:call, {:value, 1458}}
[04746]  {:pop, {:reg, 2}}
[04748]  {:pop, {:reg, 1}}
[04750]  {:pop, {:reg, 0}}
[04752]  {:set, {:reg, 0}, {:value, 4242}}
[04755]  {:set, {:reg, 1}, {:value, 25866}}
[04758]  {:set, {:reg, 2}, {:value, 32767}}
[04761]  {:set, {:reg, 3}, {:value, 28397}}
[04764]  {:call, {:value, 1841}}
[04766]  {:push, {:reg, 0}}
[04768]  {:push, {:reg, 1}}
[04770]  {:push, {:reg, 2}}
[04772]  {:set, {:reg, 0}, {:value, 28401}}
[04775]  {:set, {:reg, 1}, {:value, 1531}}
[04778]  {:add, {:reg, 2}, {:value, 16785}, {:value, 8685}}
[04782]  {:call, {:value, 1458}}
[04784]  {:pop, {:reg, 2}}
[04786]  {:pop, {:reg, 1}}
[04788]  {:pop, {:reg, 0}}
[04790]  {:pop, {:reg, 3}}
[04792]  {:pop, {:reg, 2}}
[04794]  {:pop, {:reg, 1}}
[04796]  {:pop, {:reg, 0}}
[04798]  {:ret}
[04799]  {:push, {:reg, 0}}
[04801]  {:add, {:reg, 0}, {:value, 2672}, {:value, 2}}
[04805]  {:rmem, {:reg, 0}, {:reg, 0}}
[04808]  {:jt, {:reg, 0}, {:value, 4858}}
[04811]  {:add, {:reg, 0}, {:value, 2684}, {:value, 2}}
[04815]  {:wmem, {:reg, 0}, {:value, 32767}}
[04818]  {:add, {:reg, 0}, {:value, 2672}, {:value, 2}}
[04822]  {:wmem, {:reg, 0}, {:value, 32767}}
[04825]  {:add, {:reg, 0}, {:value, 2676}, {:value, 2}}
[04829]  {:wmem, {:reg, 0}, {:value, 0}}
[04832]  {:push, {:reg, 0}}
[04834]  {:push, {:reg, 1}}
[04836]  {:push, {:reg, 2}}
[04838]  {:set, {:reg, 0}, {:value, 28453}}
[04841]  {:set, {:reg, 1}, {:value, 1531}}
[04844]  {:add, {:reg, 2}, {:value, 11174}, {:value, 5819}}
[04848]  {:call, {:value, 1458}}
[04850]  {:pop, {:reg, 2}}
[04852]  {:pop, {:reg, 1}}
[04854]  {:pop, {:reg, 0}}
[04856]  {:jmp, {:value, 4882}}
[04858]  {:push, {:reg, 0}}
[04860]  {:push, {:reg, 1}}
[04862]  {:push, {:reg, 2}}
[04864]  {:set, {:reg, 0}, {:value, 28510}}
[04867]  {:set, {:reg, 1}, {:value, 1531}}
[04870]  {:add, {:reg, 2}, {:value, 53}, {:value, 74}}
[04874]  {:call, {:value, 1458}}
[04876]  {:pop, {:reg, 2}}
[04878]  {:pop, {:reg, 1}}
[04880]  {:pop, {:reg, 0}}
[04882]  {:pop, {:reg, 0}}
[04884]  {:ret}
[04885]  {:push, {:reg, 0}}
[04887]  {:add, {:reg, 0}, {:value, 2676}, {:value, 2}}
[04891]  {:wmem, {:reg, 0}, {:value, 32767}}
[04894]  {:add, {:reg, 0}, {:value, 2680}, {:value, 2}}
[04898]  {:wmem, {:reg, 0}, {:value, 0}}
[04901]  {:push, {:reg, 0}}
[04903]  {:push, {:reg, 1}}
[04905]  {:push, {:reg, 2}}
[04907]  {:set, {:reg, 0}, {:value, 28569}}
[04910]  {:set, {:reg, 1}, {:value, 1531}}
[04913]  {:add, {:reg, 2}, {:value, 898}, {:value, 100}}
[04917]  {:call, {:value, 1458}}
[04919]  {:pop, {:reg, 2}}
[04921]  {:pop, {:reg, 1}}
[04923]  {:pop, {:reg, 0}}
[04925]  {:wmem, {:value, 2733}, {:value, 0}}
[04928]  {:pop, {:reg, 0}}
[04930]  {:ret}
[04931]  {:push, {:reg, 0}}
[04933]  {:add, {:reg, 0}, {:value, 2676}, {:value, 2}}
[04937]  {:wmem, {:reg, 0}, {:value, 0}}
[04940]  {:add, {:reg, 0}, {:value, 2680}, {:value, 2}}
[04944]  {:wmem, {:reg, 0}, {:value, 32767}}
[04947]  {:push, {:reg, 0}}
[04949]  {:push, {:reg, 1}}
[04951]  {:push, {:reg, 2}}
[04953]  {:set, {:reg, 0}, {:value, 28595}}
[04956]  {:set, {:reg, 1}, {:value, 1531}}
[04959]  {:add, {:reg, 2}, {:value, 3585}, {:value, 6892}}
[04963]  {:call, {:value, 1458}}
[04965]  {:pop, {:reg, 2}}
[04967]  {:pop, {:reg, 1}}
[04969]  {:pop, {:reg, 0}}
[04971]  {:wmem, {:value, 2733}, {:value, 0}}
[04974]  {:pop, {:reg, 0}}
[04976]  {:ret}
[04977]  {:push, {:reg, 2}}
[04979]  {:push, {:reg, 3}}
[04981]  {:rmem, {:reg, 2}, {:value, 2732}}
[04984]  {:eq, {:reg, 2}, {:reg, 2}, {:value, 2457}}
[04988]  {:jt, {:reg, 2}, {:value, 5017}}
[04991]  {:push, {:reg, 0}}
[04993]  {:push, {:reg, 1}}
[04995]  {:push, {:reg, 2}}
[04997]  {:set, {:reg, 0}, {:value, 28621}}
[05000]  {:set, {:reg, 1}, {:value, 1531}}
[05003]  {:add, {:reg, 2}, {:value, 14379}, {:value, 13501}}
[05007]  {:call, {:value, 1458}}
[05009]  {:pop, {:reg, 2}}
[05011]  {:pop, {:reg, 1}}
[05013]  {:pop, {:reg, 0}}
[05015]  {:jmp, {:value, 5336}}
[05017]  {:add, {:reg, 2}, {:reg, 0}, {:value, 2}}
[05021]  {:wmem, {:reg, 2}, {:value, 32767}}
[05024]  {:rmem, {:reg, 2}, {:value, 2462}}
[05027]  {:add, {:reg, 2}, {:reg, 2}, {:value, 27095}}
[05031]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[05035]  {:rmem, {:reg, 2}, {:reg, 2}}
[05038]  {:add, {:reg, 3}, {:value, 2457}, {:value, 1}}
[05042]  {:rmem, {:reg, 3}, {:reg, 3}}
[05045]  {:add, {:reg, 3}, {:reg, 3}, {:reg, 2}}
[05049]  {:add, {:reg, 2}, {:reg, 1}, {:value, 48}}
[05053]  {:wmem, {:reg, 3}, {:reg, 2}}
[05056]  {:rmem, {:reg, 2}, {:value, 2462}}
[05059]  {:add, {:reg, 2}, {:reg, 2}, {:value, 27101}}
[05063]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[05067]  {:wmem, {:reg, 2}, {:reg, 1}}
[05070]  {:push, {:reg, 0}}
[05072]  {:push, {:reg, 0}}
[05074]  {:push, {:reg, 1}}
[05076]  {:push, {:reg, 2}}
[05078]  {:set, {:reg, 0}, {:value, 28664}}
[05081]  {:set, {:reg, 1}, {:value, 1531}}
[05084]  {:add, {:reg, 2}, {:value, 271}, {:value, 13145}}
[05088]  {:call, {:value, 1458}}
[05090]  {:pop, {:reg, 2}}
[05092]  {:pop, {:reg, 1}}
[05094]  {:pop, {:reg, 0}}
[05096]  {:pop, {:reg, 0}}
[05098]  {:push, {:reg, 0}}
[05100]  {:add, {:reg, 2}, {:reg, 0}, {:value, 0}}
[05104]  {:rmem, {:reg, 0}, {:reg, 2}}
[05107]  {:call, {:value, 1518}}
[05109]  {:pop, {:reg, 0}}
[05111]  {:push, {:reg, 0}}
[05113]  {:push, {:reg, 0}}
[05115]  {:push, {:reg, 1}}
[05117]  {:push, {:reg, 2}}
[05119]  {:set, {:reg, 0}, {:value, 28679}}
[05122]  {:set, {:reg, 1}, {:value, 1531}}
[05125]  {:add, {:reg, 2}, {:value, 1298}, {:value, 9300}}
[05129]  {:call, {:value, 1458}}
[05131]  {:pop, {:reg, 2}}
[05133]  {:pop, {:reg, 1}}
[05135]  {:pop, {:reg, 0}}
[05137]  {:pop, {:reg, 0}}
[05139]  {:rmem, {:reg, 2}, {:value, 2462}}
[05142]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[05146]  {:wmem, {:value, 2462}, {:reg, 2}}
[05149]  {:rmem, {:reg, 3}, {:value, 27101}}
[05152]  {:eq, {:reg, 3}, {:reg, 2}, {:reg, 3}}
[05156]  {:jf, {:reg, 3}, {:value, 5336}}
[05159]  {:set, {:reg, 0}, {:value, 0}}
[05162]  {:add, {:reg, 1}, {:value, 27101}, {:value, 1}}
[05166]  {:rmem, {:reg, 1}, {:reg, 1}}
[05169]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[05173]  {:add, {:reg, 1}, {:value, 27101}, {:value, 2}}
[05177]  {:rmem, {:reg, 1}, {:reg, 1}}
[05180]  {:add, {:reg, 2}, {:value, 27101}, {:value, 3}}
[05184]  {:rmem, {:reg, 2}, {:reg, 2}}
[05187]  {:mult, {:reg, 2}, {:reg, 2}, {:reg, 2}}
[05191]  {:mult, {:reg, 1}, {:reg, 1}, {:reg, 2}}
[05195]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[05199]  {:add, {:reg, 1}, {:value, 27101}, {:value, 4}}
[05203]  {:rmem, {:reg, 1}, {:reg, 1}}
[05206]  {:mult, {:reg, 2}, {:reg, 1}, {:reg, 1}}
[05210]  {:mult, {:reg, 2}, {:reg, 2}, {:reg, 1}}
[05214]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 2}}
[05218]  {:add, {:reg, 1}, {:value, 27101}, {:value, 5}}
[05222]  {:rmem, {:reg, 1}, {:reg, 1}}
[05225]  {:mult, {:reg, 1}, {:reg, 1}, {:value, 32767}}
[05229]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[05233]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 399}}
[05237]  {:jt, {:reg, 1}, {:value, 5312}}
[05240]  {:add, {:reg, 2}, {:value, 2688}, {:value, 2}}
[05244]  {:wmem, {:reg, 2}, {:value, 2457}}
[05247]  {:add, {:reg, 2}, {:value, 2692}, {:value, 2}}
[05251]  {:wmem, {:reg, 2}, {:value, 2457}}
[05254]  {:add, {:reg, 2}, {:value, 2696}, {:value, 2}}
[05258]  {:wmem, {:reg, 2}, {:value, 2457}}
[05261]  {:add, {:reg, 2}, {:value, 2700}, {:value, 2}}
[05265]  {:wmem, {:reg, 2}, {:value, 2457}}
[05268]  {:add, {:reg, 2}, {:value, 2704}, {:value, 2}}
[05272]  {:wmem, {:reg, 2}, {:value, 2457}}
[05275]  {:wmem, {:value, 2462}, {:value, 0}}
[05278]  {:set, {:reg, 0}, {:value, 27095}}
[05281]  {:set, {:reg, 1}, {:value, 5341}}
[05284]  {:call, {:value, 1458}}
[05286]  {:push, {:reg, 0}}
[05288]  {:push, {:reg, 1}}
[05290]  {:push, {:reg, 2}}
[05292]  {:set, {:reg, 0}, {:value, 28710}}
[05295]  {:set, {:reg, 1}, {:value, 1531}}
[05298]  {:add, {:reg, 2}, {:value, 6071}, {:value, 23493}}
[05302]  {:call, {:value, 1458}}
[05304]  {:pop, {:reg, 2}}
[05306]  {:pop, {:reg, 1}}
[05308]  {:pop, {:reg, 0}}
[05310]  {:jmp, {:value, 5336}}
[05312]  {:push, {:reg, 0}}
[05314]  {:push, {:reg, 1}}
[05316]  {:push, {:reg, 2}}
[05318]  {:set, {:reg, 0}, {:value, 28777}}
[05321]  {:set, {:reg, 1}, {:value, 1531}}
[05324]  {:add, {:reg, 2}, {:value, 2876}, {:value, 15154}}
[05328]  {:call, {:value, 1458}}
[05330]  {:pop, {:reg, 2}}
[05332]  {:pop, {:reg, 1}}
[05334]  {:pop, {:reg, 0}}
[05336]  {:pop, {:reg, 3}}
[05338]  {:pop, {:reg, 2}}
[05340]  {:ret}
[05341]  {:push, {:reg, 2}}
[05343]  {:add, {:reg, 2}, {:value, 2457}, {:value, 1}}
[05347]  {:rmem, {:reg, 2}, {:reg, 2}}
[05350]  {:add, {:reg, 2}, {:reg, 2}, {:reg, 0}}
[05354]  {:wmem, {:reg, 2}, {:value, 95}}
[05357]  {:pop, {:reg, 2}}
[05359]  {:ret}
[05360]  {:push, {:reg, 0}}
[05362]  {:push, {:reg, 1}}
[05364]  {:set, {:reg, 0}, {:value, 2688}}
[05367]  {:set, {:reg, 1}, {:value, 2}}
[05370]  {:call, {:value, 4977}}
[05372]  {:pop, {:reg, 1}}
[05374]  {:pop, {:reg, 0}}
[05376]  {:ret}
[05377]  {:push, {:reg, 0}}
[05379]  {:push, {:reg, 1}}
[05381]  {:set, {:reg, 0}, {:value, 2692}}
[05384]  {:set, {:reg, 1}, {:value, 3}}
[05387]  {:call, {:value, 4977}}
[05389]  {:pop, {:reg, 1}}
[05391]  {:pop, {:reg, 0}}
[05393]  {:ret}
[05394]  {:push, {:reg, 0}}
[05396]  {:push, {:reg, 1}}
[05398]  {:set, {:reg, 0}, {:value, 2696}}
[05401]  {:set, {:reg, 1}, {:value, 5}}
[05404]  {:call, {:value, 4977}}
[05406]  {:pop, {:reg, 1}}
[05408]  {:pop, {:reg, 0}}
[05410]  {:ret}
[05411]  {:push, {:reg, 0}}
[05413]  {:push, {:reg, 1}}
[05415]  {:set, {:reg, 0}, {:value, 2700}}
[05418]  {:set, {:reg, 1}, {:value, 7}}
[05421]  {:call, {:value, 4977}}
[05423]  {:pop, {:reg, 1}}
[05425]  {:pop, {:reg, 0}}
[05427]  {:ret}
[05428]  {:push, {:reg, 0}}
[05430]  {:push, {:reg, 1}}
[05432]  {:set, {:reg, 0}, {:value, 2704}}
[05435]  {:set, {:reg, 1}, {:value, 9}}
[05438]  {:call, {:value, 4977}}
[05440]  {:pop, {:reg, 1}}
[05442]  {:pop, {:reg, 0}}
[05444]  {:ret}
[05445]  {:push, {:reg, 0}}				# Start of teleporter USE function
[05447]  {:push, {:reg, 1}}
[05449]  {:push, {:reg, 2}}
[05451]  {:jf, {:reg, 7}, {:value, 5605}}				# "Test if teleporter has register 7 set to 0"
[05454]  {:push, {:reg, 0}}
[05456]  {:push, {:reg, 1}}
[05458]  {:push, {:reg, 2}}
[05460]  {:set, {:reg, 0}, {:value, 28844}}
[05463]  {:set, {:reg, 1}, {:value, 1531}}
[05466]  {:add, {:reg, 2}, {:value, 1475}, {:value, 12831}}
[05470]  {:call, {:value, 1458}}
[05472]  {:pop, {:reg, 2}}
[05474]  {:pop, {:reg, 1}}
[05476]  {:pop, {:reg, 0}}
[05478]  {:noop}
[05479]  {:noop}
[05480]  {:noop}
[05481]  {:noop}
[05482]  {:noop}
[05483]  {:set, {:reg, 0}, {:value, 4}}				# args for ackerman(4, 1)
[05486]  {:set, {:reg, 1}, {:value, 1}}
[05489]  {:call, {:value, 6027}}				# Call teleporter ackermann function
[05491]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 6}}				# R0 must be 6 to pass confirmation check
[05495]  {:jf, {:reg, 1}, {:value, 5579}}
[05498]  {:push, {:reg, 0}}
[05500]  {:push, {:reg, 1}}
[05502]  {:push, {:reg, 2}}
[05504]  {:set, {:reg, 0}, {:value, 29014}}
[05507]  {:set, {:reg, 1}, {:value, 1531}}
[05510]  {:add, {:reg, 2}, {:value, 9025}, {:value, 690}}
[05514]  {:call, {:value, 1458}}				# Print: `You wake up on a sandy beach...`
[05516]  {:pop, {:reg, 2}}
[05518]  {:pop, {:reg, 1}}
[05520]  {:pop, {:reg, 0}}
[05522]  {:set, {:reg, 0}, {:reg, 7}}				# Put teleporter register in r0
[05525]  {:set, {:reg, 1}, {:value, 25866}}
[05528]  {:set, {:reg, 2}, {:value, 32767}}
[05531]  {:push, {:reg, 3}}
[05533]  {:set, {:reg, 3}, {:value, 29241}}
[05536]  {:call, {:value, 1841}}				# Print island teleport code
[05538]  {:pop, {:reg, 3}}
[05540]  {:push, {:reg, 0}}
[05542]  {:push, {:reg, 1}}
[05544]  {:push, {:reg, 2}}
[05546]  {:set, {:reg, 0}, {:value, 29245}}
[05549]  {:set, {:reg, 1}, {:value, 1531}}
[05552]  {:add, {:reg, 2}, {:value, 16808}, {:value, 10433}}
[05556]  {:call, {:value, 1458}}
[05558]  {:pop, {:reg, 2}}				# Print `It begins to rain` message
[05560]  {:pop, {:reg, 1}}
[05562]  {:pop, {:reg, 0}}
[05564]  {:wmem, {:value, 2732}, {:value, 2498}}				# Set current room to beach
[05567]  {:wmem, {:value, 2733}, {:value, 0}}
[05570]  {:add, {:reg, 1}, {:value, 2708}, {:value, 2}}
[05574]  {:wmem, {:reg, 1}, {:value, 32767}}				# Set the location of the teleporter to a non-existant room
[05577]  {:jmp, {:value, 5714}}				# Jump to return
[05579]  {:push, {:reg, 0}}				# Target of jump if confirmation function fails
[05581]  {:push, {:reg, 1}}
[05583]  {:push, {:reg, 2}}
[05585]  {:set, {:reg, 0}, {:value, 29400}}
[05588]  {:set, {:reg, 1}, {:value, 1531}}
[05591]  {:add, {:reg, 2}, {:value, 25133}, {:value, 3901}}
[05595]  {:call, {:value, 1458}}
[05597]  {:pop, {:reg, 2}}
[05599]  {:pop, {:reg, 1}}
[05601]  {:pop, {:reg, 0}}
[05603]  {:jmp, {:value, 5714}}				# Jump to return
[05605]  {:push, {:reg, 0}}				# Target of jump if teleport register is 0
[05607]  {:push, {:reg, 1}}
[05609]  {:push, {:reg, 2}}
[05611]  {:set, {:reg, 0}, {:value, 29545}}
[05614]  {:set, {:reg, 1}, {:value, 1531}}
[05617]  {:add, {:reg, 2}, {:value, 8342}, {:value, 18380}}
[05621]  {:call, {:value, 1458}}
[05623]  {:pop, {:reg, 2}}
[05625]  {:pop, {:reg, 1}}
[05627]  {:pop, {:reg, 0}}
[05629]  {:set, {:reg, 0}, {:value, 0}}
[05632]  {:add, {:reg, 2}, {:value, 1}, {:value, 27101}}
[05636]  {:rmem, {:reg, 1}, {:reg, 2}}
[05639]  {:add, {:reg, 0}, {:reg, 0}, {:reg, 1}}
[05643]  {:mult, {:reg, 0}, {:reg, 0}, {:value, 31660}}
[05647]  {:call, {:value, 2125}}
[05649]  {:rmem, {:reg, 1}, {:value, 27101}}
[05652]  {:add, {:reg, 1}, {:reg, 1}, {:value, 27101}}
[05656]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[05660]  {:gt, {:reg, 1}, {:reg, 2}, {:reg, 1}}
[05664]  {:jf, {:reg, 1}, {:value, 5636}}
[05667]  {:set, {:reg, 1}, {:value, 25866}}
[05670]  {:set, {:reg, 2}, {:value, 32767}}
[05673]  {:push, {:reg, 3}}
[05675]  {:set, {:reg, 3}, {:value, 29663}}
[05678]  {:call, {:value, 1841}}
[05680]  {:pop, {:reg, 3}}
[05682]  {:push, {:reg, 0}}
[05684]  {:push, {:reg, 1}}
[05686]  {:push, {:reg, 2}}
[05688]  {:set, {:reg, 0}, {:value, 29667}}
[05691]  {:set, {:reg, 1}, {:value, 1531}}
[05694]  {:add, {:reg, 2}, {:value, 26729}, {:value, 1158}}
[05698]  {:call, {:value, 1458}}
[05700]  {:pop, {:reg, 2}}
[05702]  {:pop, {:reg, 1}}
[05704]  {:pop, {:reg, 0}}
[05706]  {:wmem, {:value, 2732}, {:value, 2488}}				# Set room ptr back to Synacor HQ
[05709]  {:wmem, {:value, 2733}, {:value, 0}}
[05712]  {:jmp, {:value, 5714}}
[05714]  {:pop, {:reg, 2}}
[05716]  {:pop, {:reg, 1}}
[05718]  {:pop, {:reg, 0}}
[05720]  {:ret}
[05721]  {:push, {:reg, 0}}
[05723]  {:push, {:reg, 1}}
[05725]  {:push, {:reg, 2}}
[05727]  {:push, {:reg, 3}}
[05729]  {:push, {:reg, 0}}
[05731]  {:push, {:reg, 1}}
[05733]  {:push, {:reg, 2}}
[05735]  {:set, {:reg, 0}, {:value, 29757}}
[05738]  {:set, {:reg, 1}, {:value, 1531}}
[05741]  {:add, {:reg, 2}, {:value, 28931}, {:value, 3696}}
[05745]  {:call, {:value, 1458}}
[05747]  {:pop, {:reg, 2}}
[05749]  {:pop, {:reg, 1}}
[05751]  {:pop, {:reg, 0}}
[05753]  {:rmem, {:reg, 0}, {:value, 3955}}
[05756]  {:rmem, {:reg, 1}, {:value, 3956}}
[05759]  {:call, {:value, 2125}}
[05761]  {:rmem, {:reg, 1}, {:value, 3957}}
[05764]  {:call, {:value, 2125}}
[05766]  {:set, {:reg, 1}, {:value, 25919}}
[05769]  {:set, {:reg, 2}, {:value, 4}}
[05772]  {:push, {:reg, 3}}
[05774]  {:set, {:reg, 3}, {:value, 29942}}
[05777]  {:call, {:value, 1841}}
[05779]  {:pop, {:reg, 3}}
[05781]  {:push, {:reg, 0}}
[05783]  {:push, {:reg, 1}}
[05785]  {:push, {:reg, 2}}
[05787]  {:set, {:reg, 0}, {:value, 29946}}
[05790]  {:set, {:reg, 1}, {:value, 1531}}
[05793]  {:add, {:reg, 2}, {:value, 14595}, {:value, 4719}}
[05797]  {:call, {:value, 1458}}
[05799]  {:pop, {:reg, 2}}
[05801]  {:pop, {:reg, 1}}
[05803]  {:pop, {:reg, 0}}
[05805]  {:pop, {:reg, 3}}
[05807]  {:pop, {:reg, 2}}
[05809]  {:pop, {:reg, 1}}
[05811]  {:pop, {:reg, 0}}
[05813]  {:ret}
[05814]  {:out, {:value, '- '}}				# insert '-' before printing char
[05818]  {:call, {:value, 1518}}
[05820]  {:out_newline, {:value, :newline}}
[05822]  {:ret}
[05823]  {:push, {:reg, 1}}
[05825]  {:push, {:reg, 2}}
[05827]  {:set, {:reg, 0}, {:value, 27381}}
[05830]  {:set, {:reg, 1}, {:value, 5846}}
[05833]  {:set, {:reg, 2}, {:value, 0}}
[05836]  {:call, {:value, 1458}}
[05838]  {:set, {:reg, 0}, {:reg, 2}}
[05841]  {:pop, {:reg, 2}}
[05843]  {:pop, {:reg, 1}}
[05845]  {:ret}
[05846]  {:push, {:reg, 3}}
[05848]  {:push, {:reg, 4}}
[05850]  {:rmem, {:reg, 3}, {:value, 2732}}
[05853]  {:add, {:reg, 4}, {:reg, 0}, {:value, 2}}
[05857]  {:rmem, {:reg, 4}, {:reg, 4}}
[05860]  {:eq, {:reg, 3}, {:reg, 3}, {:reg, 4}}
[05864]  {:jf, {:reg, 3}, {:value, 5871}}
[05867]  {:add, {:reg, 2}, {:reg, 2}, {:value, 1}}
[05871]  {:pop, {:reg, 4}}
[05873]  {:pop, {:reg, 3}}
[05875]  {:ret}
[05876]  {:push, {:reg, 0}}				# Print things of interest function
[05878]  {:push, {:reg, 1}}
[05880]  {:set, {:reg, 0}, {:value, 27381}}
[05883]  {:set, {:reg, 1}, {:value, 5893}}
[05886]  {:call, {:value, 1458}}
[05888]  {:pop, {:reg, 1}}
[05890]  {:pop, {:reg, 0}}
[05892]  {:ret}
[05893]  {:push, {:reg, 3}}
[05895]  {:add, {:reg, 3}, {:reg, 0}, {:value, 2}}
[05899]  {:rmem, {:reg, 3}, {:reg, 3}}
[05902]  {:eq, {:reg, 3}, {:reg, 2}, {:reg, 3}}
[05906]  {:jf, {:reg, 3}, {:value, 5918}}
[05909]  {:add, {:reg, 0}, {:reg, 0}, {:value, 0}}
[05913]  {:rmem, {:reg, 0}, {:reg, 0}}
[05916]  {:call, {:value, 5814}}
[05918]  {:pop, {:reg, 3}}
[05920]  {:ret}
[05921]  {:push, {:reg, 1}}
[05923]  {:push, {:reg, 2}}
[05925]  {:set, {:reg, 2}, {:reg, 0}}
[05928]  {:set, {:reg, 0}, {:value, 27381}}
[05931]  {:set, {:reg, 1}, {:value, 5964}}
[05934]  {:call, {:value, 1543}}
[05936]  {:eq, {:reg, 1}, {:reg, 0}, {:value, 32767}}
[05940]  {:jt, {:reg, 1}, {:value, 5956}}
[05943]  {:add, {:reg, 1}, {:value, 27381}, {:reg, 0}}
[05947]  {:add, {:reg, 1}, {:reg, 1}, {:value, 1}}
[05951]  {:rmem, {:reg, 0}, {:reg, 1}}
[05954]  {:jmp, {:value, 5959}}
[05956]  {:set, {:reg, 0}, {:value, 0}}
[05959]  {:pop, {:reg, 2}}
[05961]  {:pop, {:reg, 1}}
[05963]  {:ret}
[05964]  {:push, {:reg, 1}}
[05966]  {:set, {:reg, 1}, {:reg, 2}}
[05969]  {:add, {:reg, 0}, {:reg, 0}, {:value, 0}}
[05973]  {:rmem, {:reg, 0}, {:reg, 0}}
[05976]  {:call, {:value, 1667}}
[05978]  {:pop, {:reg, 1}}
[05980]  {:jf, {:reg, 0}, {:value, 5989}}
[05983]  {:set, {:reg, 2}, {:reg, 1}}
[05986]  {:set, {:reg, 1}, {:value, 32767}}
[05989]  {:ret}
[05990]  {:push, {:reg, 1}}
[05992]  {:push, {:reg, 2}}
[05994]  {:add, {:reg, 0}, {:reg, 0}, {:value, 2}}
[05998]  {:rmem, {:reg, 0}, {:reg, 0}}
[06001]  {:jf, {:reg, 0}, {:value, 6019}}
[06004]  {:rmem, {:reg, 1}, {:value, 2732}}
[06007]  {:eq, {:reg, 1}, {:reg, 0}, {:reg, 1}}
[06011]  {:jt, {:reg, 1}, {:value, 6019}}
[06014]  {:set, {:reg, 0}, {:value, 0}}
[06017]  {:jmp, {:value, 6022}}
[06019]  {:set, {:reg, 0}, {:value, 1}}
[06022]  {:pop, {:reg, 2}}
[06024]  {:pop, {:reg, 1}}
[06026]  {:ret}
[06027]  {:ret}				# Teleporter confirmation function
[06028]  {:unknown, [32768]}
[06029]  {:unknown, [6035]}
[06030]  {:add, {:reg, 0}, {:reg, 1}, {:value, 1}}
[06034]  {:ret}
[06035]  {:jt, {:reg, 1}, {:value, 6048}}
[06038]  {:add, {:reg, 0}, {:reg, 0}, {:value, 32767}}
[06042]  {:set, {:reg, 1}, {:reg, 7}}
[06045]  {:call, {:value, 6027}}
[06047]  {:ret}
[06048]  {:push, {:reg, 0}}
[06050]  {:add, {:reg, 1}, {:reg, 1}, {:value, 32767}}
[06054]  {:call, {:value, 6027}}
[06056]  {:set, {:reg, 1}, {:reg, 0}}
[06059]  {:pop, {:reg, 0}}
[06061]  {:add, {:reg, 0}, {:reg, 0}, {:value, 32767}}
[06065]  {:call, {:value, 6027}}
[06067]  {:ret}
[06068]  {:mod, {:value, 84}, {:value, 101}, {:value, 115}}
[06072]  {:unknown, 't'}
[06073]  {:unknown, ' '}
[06074]  {:unknown, 's'}
[06075]  {:unknown, 't'}
[06076]  {:unknown, 'r'}
[06077]  {:unknown, 'i'}
[06078]  {:unknown, 'n'}
[06079]  {:unknown, 'g'}
[06080]  {:unknown, '#'}
[06081]  {:unknown, 's'}
[06082]  {:unknown, 'e'}
[06083]  {:unknown, 'l'}
[06084]  {:unknown, 'f'}
[06085]  {:unknown, '-'}
[06086]  {:unknown, 't'}
[06087]  {:unknown, 'e'}
[06088]  {:unknown, 's'}
[06089]  {:unknown, 't'}
[06090]  {:unknown, ' '}
[06091]  {:unknown, 'c'}
[06092]  {:unknown, 'o'}
[06093]  {:unknown, 'm'}
[06094]  {:unknown, 'p'}
[06095]  {:unknown, 'l'}
[06096]  {:unknown, 'e'}
[06097]  {:unknown, 't'}
[06098]  {:unknown, 'e'}
[06099]  {:unknown, ','}
[06100]  {:unknown, ' '}
[06101]  {:unknown, 'a'}
[06102]  {:unknown, 'l'}
[06103]  {:unknown, 'l'}
[06104]  {:unknown, ' '}
[06105]  {:unknown, 't'}
[06106]  {:unknown, 'e'}
[06107]  {:unknown, 's'}
[06108]  {:unknown, 't'}
[06109]  {:unknown, 's'}
[06110]  {:unknown, ' '}
[06111]  {:unknown, 'p'}
[06112]  {:unknown, 'a'}
[06113]  {:unknown, 's'}
[06114]  {:unknown, 's'}
[06115]  {:mult, {:value, 8}, {:value, 99}, {:value, 111}}
[06119]  {:unknown, 'm'}
[06120]  {:unknown, 'p'}
[06121]  {:unknown, 'l'}
[06122]  {:unknown, 'e'}
[06123]  {:unknown, 't'}
[06124]  {:unknown, 'e'}
[06125]  {:pop, {:value, 1248}}
[06127]  {:unknown, [32717]}
[06128]  {:unknown, [18224]}
[06129]  {:and, {:value, 120}, {:value, 79}, {:value, 108}}				# Start of teleporter code
[06133]  {:unknown, 'y'}
[06134]  {:unknown, 'L'}
[06135]  {:unknown, 'e'}
[06136]  {:unknown, 'D'}
[06137]  {:unknown, 'Q'}
[06138]  {:unknown, 'b'}
[06139]  {:unknown, 'g'}
[06140]  {:unknown, 'h'}
[06141]  {:unknown, 'E'}
[06142]  {:add, {:value, 70}, {:value, 111}, {:value, 111}}
[06146]  {:unknown, 't'}
[06147]  {:unknown, 'h'}
[06148]  {:unknown, 'i'}
[06149]  {:unknown, 'l'}
[06150]  {:unknown, 'l'}
[06151]  {:unknown, 's'}
[06152]  {:unknown, [183]}
[06153]  {:unknown, 'Y'}
[06154]  {:unknown, 'o'}
[06155]  {:unknown, 'u'}
[06156]  {:unknown, ' '}
[06157]  {:unknown, 'f'}
[06158]  {:unknown, 'i'}
[06159]  {:unknown, 'n'}
[06160]  {:unknown, 'd'}
[06161]  {:unknown, ' '}
[06162]  {:unknown, 'y'}
[06163]  {:unknown, 'o'}
[06164]  {:unknown, 'u'}
[06165]  {:unknown, 'r'}
[06166]  {:unknown, 's'}
[06167]  {:unknown, 'e'}
[06168]  {:unknown, 'l'}
[06169]  {:unknown, 'f'}
[06170]  {:unknown, ' '}
[06171]  {:unknown, 's'}
[06172]  {:unknown, 't'}
[06173]  {:unknown, 'a'}
[06174]  {:unknown, 'n'}
[06175]  {:unknown, 'd'}
[06176]  {:unknown, 'i'}
[06177]  {:unknown, 'n'}
[06178]  {:unknown, 'g'}
[06179]  {:unknown, ' '}
[06180]  {:unknown, 'a'}
[06181]  {:unknown, 't'}
[06182]  {:unknown, ' '}
[06183]  {:unknown, 't'}
[06184]  {:unknown, 'h'}
[06185]  {:unknown, 'e'}
[06186]  {:unknown, ' '}
[06187]  {:unknown, 'b'}
[06188]  {:unknown, 'a'}
[06189]  {:unknown, 's'}
[06190]  {:unknown, 'e'}
[06191]  {:unknown, ' '}
[06192]  {:unknown, 'o'}
[06193]  {:unknown, 'f'}
[06194]  {:unknown, ' '}
[06195]  {:unknown, 'a'}
[06196]  {:unknown, 'n'}
[06197]  {:unknown, ' '}
[06198]  {:unknown, 'e'}
[06199]  {:unknown, 'n'}
[06200]  {:unknown, 'o'}
[06201]  {:unknown, 'r'}
[06202]  {:unknown, 'm'}
[06203]  {:unknown, 'o'}
[06204]  {:unknown, 'u'}
[06205]  {:unknown, 's'}
[06206]  {:unknown, ' '}
[06207]  {:unknown, 'm'}
[06208]  {:unknown, 'o'}
[06209]  {:unknown, 'u'}
[06210]  {:unknown, 'n'}
[06211]  {:unknown, 't'}
[06212]  {:unknown, 'a'}
[06213]  {:unknown, 'i'}
[06214]  {:unknown, 'n'}
[06215]  {:unknown, '.'}
[06216]  {:unknown, ' '}
[06217]  {:unknown, ' '}
[06218]  {:unknown, 'A'}
[06219]  {:unknown, 't'}
[06220]  {:unknown, ' '}
[06221]  {:unknown, 'i'}
[06222]  {:unknown, 't'}
[06223]  {:unknown, 's'}
[06224]  {:unknown, ' '}
[06225]  {:unknown, 'b'}
[06226]  {:unknown, 'a'}
[06227]  {:unknown, 's'}
[06228]  {:unknown, 'e'}
[06229]  {:unknown, ' '}
[06230]  {:unknown, 't'}
[06231]  {:unknown, 'o'}
[06232]  {:unknown, ' '}
[06233]  {:unknown, 't'}
[06234]  {:unknown, 'h'}
[06235]  {:unknown, 'e'}
[06236]  {:unknown, ' '}
[06237]  {:unknown, 'n'}
[06238]  {:unknown, 'o'}
[06239]  {:unknown, 'r'}
[06240]  {:unknown, 't'}
[06241]  {:unknown, 'h'}
[06242]  {:unknown, ','}
[06243]  {:unknown, ' '}
[06244]  {:unknown, 't'}
[06245]  {:unknown, 'h'}
[06246]  {:unknown, 'e'}
[06247]  {:unknown, 'r'}
[06248]  {:unknown, 'e'}
[06249]  {:unknown, ' '}
[06250]  {:unknown, 'i'}
[06251]  {:unknown, 's'}
[06252]  {:unknown, ' '}
[06253]  {:unknown, 'a'}
[06254]  {:unknown, ' '}
[06255]  {:unknown, 'm'}
[06256]  {:unknown, 'a'}
[06257]  {:unknown, 's'}
[06258]  {:unknown, 's'}
[06259]  {:unknown, 'i'}
[06260]  {:unknown, 'v'}
[06261]  {:unknown, 'e'}
[06262]  {:unknown, ' '}
[06263]  {:unknown, 'd'}
[06264]  {:unknown, 'o'}
[06265]  {:unknown, 'o'}
[06266]  {:unknown, 'r'}
[06267]  {:unknown, 'w'}
[06268]  {:unknown, 'a'}
[06269]  {:unknown, 'y'}
[06270]  {:unknown, '.'}
[06271]  {:unknown, ' '}
[06272]  {:unknown, ' '}
[06273]  {:unknown, 'A'}
[06274]  {:unknown, ' '}
[06275]  {:unknown, 's'}
[06276]  {:unknown, 'i'}
[06277]  {:unknown, 'g'}
[06278]  {:unknown, 'n'}
[06279]  {:unknown, ' '}
[06280]  {:unknown, 'n'}
[06281]  {:unknown, 'e'}
[06282]  {:unknown, 'a'}
[06283]  {:unknown, 'r'}
[06284]  {:unknown, 'b'}
[06285]  {:unknown, 'y'}
[06286]  {:unknown, ' '}
[06287]  {:unknown, 'r'}
[06288]  {:unknown, 'e'}
[06289]  {:unknown, 'a'}
[06290]  {:unknown, 'd'}
[06291]  {:unknown, 's'}
[06292]  {:unknown, ' '}
[06293]  {:unknown, '"'}
[06294]  {:unknown, 'K'}
[06295]  {:unknown, 'e'}
[06296]  {:unknown, 'e'}
[06297]  {:unknown, 'p'}
[06298]  {:unknown, ' '}
[06299]  {:unknown, 'o'}
[06300]  {:unknown, 'u'}
[06301]  {:unknown, 't'}
[06302]  {:unknown, '!'}
[06303]  {:unknown, ' '}
[06304]  {:unknown, ' '}
[06305]  {:unknown, 'D'}
[06306]  {:unknown, 'e'}
[06307]  {:unknown, 'f'}
[06308]  {:unknown, 'i'}
[06309]  {:unknown, 'n'}
[06310]  {:unknown, 'i'}
[06311]  {:unknown, 't'}
[06312]  {:unknown, 'e'}
[06313]  {:unknown, 'l'}
[06314]  {:unknown, 'y'}
[06315]  {:unknown, ' '}
[06316]  {:unknown, 'n'}
[06317]  {:unknown, 'o'}
[06318]  {:unknown, ' '}
[06319]  {:unknown, 't'}
[06320]  {:unknown, 'r'}
[06321]  {:unknown, 'e'}
[06322]  {:unknown, 'a'}
[06323]  {:unknown, 's'}
[06324]  {:unknown, 'u'}
[06325]  {:unknown, 'r'}
[06326]  {:unknown, 'e'}
[06327]  {:unknown, ' '}
[06328]  {:unknown, 'w'}
[06329]  {:unknown, 'i'}
[06330]  {:unknown, 't'}
[06331]  {:unknown, 'h'}
[06332]  {:unknown, 'i'}
[06333]  {:unknown, 'n'}
[06334]  {:unknown, '!'}
[06335]  {:unknown, '"'}
[06336]  {:jt, {:value, 100}, {:value, 111}}
[06339]  {:unknown, 'o'}
[06340]  {:unknown, 'r'}
[06341]  {:unknown, 'w'}
[06342]  {:unknown, 'a'}
[06343]  {:unknown, 'y'}
[06344]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[06348]  {:unknown, 't'}
[06349]  {:unknown, 'h'}
[06350]  {:add, {:value, 70}, {:value, 111}, {:value, 111}}
[06354]  {:unknown, 't'}
[06355]  {:unknown, 'h'}
[06356]  {:unknown, 'i'}
[06357]  {:unknown, 'l'}
[06358]  {:unknown, 'l'}
[06359]  {:unknown, 's'}
[06360]  {:unknown, 'J'}
[06361]  {:unknown, 'A'}
[06362]  {:unknown, 's'}
[06363]  {:unknown, ' '}
[06364]  {:unknown, 'y'}
[06365]  {:unknown, 'o'}
[06366]  {:unknown, 'u'}
[06367]  {:unknown, ' '}
[06368]  {:unknown, 'b'}
[06369]  {:unknown, 'e'}
[06370]  {:unknown, 'g'}
[06371]  {:unknown, 'i'}
[06372]  {:unknown, 'n'}
[06373]  {:unknown, ' '}
[06374]  {:unknown, 't'}
[06375]  {:unknown, 'o'}
[06376]  {:unknown, ' '}
[06377]  {:unknown, 'l'}
[06378]  {:unknown, 'e'}
[06379]  {:unknown, 'a'}
[06380]  {:unknown, 'v'}
[06381]  {:unknown, 'e'}
[06382]  {:unknown, ','}
[06383]  {:unknown, ' '}
[06384]  {:unknown, 'y'}
[06385]  {:unknown, 'o'}
[06386]  {:unknown, 'u'}
[06387]  {:unknown, ' '}
[06388]  {:unknown, 'f'}
[06389]  {:unknown, 'e'}
[06390]  {:unknown, 'e'}
[06391]  {:unknown, 'l'}
[06392]  {:unknown, ' '}
[06393]  {:unknown, 't'}
[06394]  {:unknown, 'h'}
[06395]  {:unknown, 'e'}
[06396]  {:unknown, ' '}
[06397]  {:unknown, 'u'}
[06398]  {:unknown, 'r'}
[06399]  {:unknown, 'g'}
[06400]  {:unknown, 'e'}
[06401]  {:unknown, ' '}
[06402]  {:unknown, 'f'}
[06403]  {:unknown, 'o'}
[06404]  {:unknown, 'r'}
[06405]  {:unknown, ' '}
[06406]  {:unknown, 'a'}
[06407]  {:unknown, 'd'}
[06408]  {:unknown, 'v'}
[06409]  {:unknown, 'e'}
[06410]  {:unknown, 'n'}
[06411]  {:unknown, 't'}
[06412]  {:unknown, 'u'}
[06413]  {:unknown, 'r'}
[06414]  {:unknown, 'e'}
[06415]  {:unknown, ' '}
[06416]  {:unknown, 'p'}
[06417]  {:unknown, 'u'}
[06418]  {:unknown, 'l'}
[06419]  {:unknown, 'l'}
[06420]  {:unknown, 'i'}
[06421]  {:unknown, 'n'}
[06422]  {:unknown, 'g'}
[06423]  {:unknown, ' '}
[06424]  {:unknown, 'y'}
[06425]  {:unknown, 'o'}
[06426]  {:unknown, 'u'}
[06427]  {:unknown, ' '}
[06428]  {:unknown, 'b'}
[06429]  {:unknown, 'a'}
[06430]  {:unknown, 'c'}
[06431]  {:unknown, 'k'}
[06432]  {:unknown, '.'}
[06433]  {:unknown, '.'}
[06434]  {:unknown, '.'}
[06435]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[06439]  {:unknown, 't'}
[06440]  {:unknown, 'h'}
[06441]  {:add, {:value, 68}, {:value, 97}, {:value, 114}}
[06445]  {:unknown, 'k'}
[06446]  {:unknown, ' '}
[06447]  {:unknown, 'c'}
[06448]  {:unknown, 'a'}
[06449]  {:unknown, 'v'}
[06450]  {:unknown, 'e'}
[06451]  {:unknown, [133]}
[06452]  {:unknown, 'T'}
[06453]  {:unknown, 'h'}
[06454]  {:unknown, 'i'}
[06455]  {:unknown, 's'}
[06456]  {:unknown, ' '}
[06457]  {:unknown, 's'}
[06458]  {:unknown, 'e'}
[06459]  {:unknown, 'e'}
[06460]  {:unknown, 'm'}
[06461]  {:unknown, 's'}
[06462]  {:unknown, ' '}
[06463]  {:unknown, 't'}
[06464]  {:unknown, 'o'}
[06465]  {:unknown, ' '}
[06466]  {:unknown, 'b'}
[06467]  {:unknown, 'e'}
[06468]  {:unknown, ' '}
[06469]  {:unknown, 't'}
[06470]  {:unknown, 'h'}
[06471]  {:unknown, 'e'}
[06472]  {:unknown, ' '}
[06473]  {:unknown, 'm'}
[06474]  {:unknown, 'o'}
[06475]  {:unknown, 'u'}
[06476]  {:unknown, 't'}
[06477]  {:unknown, 'h'}
[06478]  {:unknown, ' '}
[06479]  {:unknown, 'o'}
[06480]  {:unknown, 'f'}
[06481]  {:unknown, ' '}
[06482]  {:unknown, 'a'}
[06483]  {:unknown, ' '}
[06484]  {:unknown, 'd'}
[06485]  {:unknown, 'e'}
[06486]  {:unknown, 'e'}
[06487]  {:unknown, 'p'}
[06488]  {:unknown, ' '}
[06489]  {:unknown, 'c'}
[06490]  {:unknown, 'a'}
[06491]  {:unknown, 'v'}
[06492]  {:unknown, 'e'}
[06493]  {:unknown, '.'}
[06494]  {:unknown, ' '}
[06495]  {:unknown, ' '}
[06496]  {:unknown, 'A'}
[06497]  {:unknown, 's'}
[06498]  {:unknown, ' '}
[06499]  {:unknown, 'y'}
[06500]  {:unknown, 'o'}
[06501]  {:unknown, 'u'}
[06502]  {:unknown, ' '}
[06503]  {:unknown, 'p'}
[06504]  {:unknown, 'e'}
[06505]  {:unknown, 'e'}
[06506]  {:unknown, 'r'}
[06507]  {:unknown, ' '}
[06508]  {:unknown, 'n'}
[06509]  {:unknown, 'o'}
[06510]  {:unknown, 'r'}
[06511]  {:unknown, 't'}
[06512]  {:unknown, 'h'}
[06513]  {:unknown, ' '}
[06514]  {:unknown, 'i'}
[06515]  {:unknown, 'n'}
[06516]  {:unknown, 't'}
[06517]  {:unknown, 'o'}
[06518]  {:unknown, ' '}
[06519]  {:unknown, 't'}
[06520]  {:unknown, 'h'}
[06521]  {:unknown, 'e'}
[06522]  {:unknown, ' '}
[06523]  {:unknown, 'd'}
[06524]  {:unknown, 'a'}
[06525]  {:unknown, 'r'}
[06526]  {:unknown, 'k'}
[06527]  {:unknown, 'n'}
[06528]  {:unknown, 'e'}
[06529]  {:unknown, 's'}
[06530]  {:unknown, 's'}
[06531]  {:unknown, ','}
[06532]  {:unknown, ' '}
[06533]  {:unknown, 'y'}
[06534]  {:unknown, 'o'}
[06535]  {:unknown, 'u'}
[06536]  {:unknown, ' '}
[06537]  {:unknown, 't'}
[06538]  {:unknown, 'h'}
[06539]  {:unknown, 'i'}
[06540]  {:unknown, 'n'}
[06541]  {:unknown, 'k'}
[06542]  {:unknown, ' '}
[06543]  {:unknown, 'y'}
[06544]  {:unknown, 'o'}
[06545]  {:unknown, 'u'}
[06546]  {:unknown, ' '}
[06547]  {:unknown, 'h'}
[06548]  {:unknown, 'e'}
[06549]  {:unknown, 'a'}
[06550]  {:unknown, 'r'}
[06551]  {:unknown, ' '}
[06552]  {:unknown, 't'}
[06553]  {:unknown, 'h'}
[06554]  {:unknown, 'e'}
[06555]  {:unknown, ' '}
[06556]  {:unknown, 'e'}
[06557]  {:unknown, 'c'}
[06558]  {:unknown, 'h'}
[06559]  {:unknown, 'o'}
[06560]  {:unknown, 'e'}
[06561]  {:unknown, 's'}
[06562]  {:unknown, ' '}
[06563]  {:unknown, 'o'}
[06564]  {:unknown, 'f'}
[06565]  {:unknown, ' '}
[06566]  {:unknown, 'b'}
[06567]  {:unknown, 'a'}
[06568]  {:unknown, 't'}
[06569]  {:unknown, 's'}
[06570]  {:unknown, ' '}
[06571]  {:unknown, 'd'}
[06572]  {:unknown, 'e'}
[06573]  {:unknown, 'e'}
[06574]  {:unknown, 'p'}
[06575]  {:unknown, 'e'}
[06576]  {:unknown, 'r'}
[06577]  {:unknown, ' '}
[06578]  {:unknown, 'w'}
[06579]  {:unknown, 'i'}
[06580]  {:unknown, 't'}
[06581]  {:unknown, 'h'}
[06582]  {:unknown, 'i'}
[06583]  {:unknown, 'n'}
[06584]  {:unknown, '.'}
[06585]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[06589]  {:unknown, 't'}
[06590]  {:unknown, 'h'}
[06591]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[06595]  {:unknown, 't'}
[06596]  {:unknown, 'h'}
[06597]  {:add, {:value, 68}, {:value, 97}, {:value, 114}}
[06601]  {:unknown, 'k'}
[06602]  {:unknown, ' '}
[06603]  {:unknown, 'c'}
[06604]  {:unknown, 'a'}
[06605]  {:unknown, 'v'}
[06606]  {:unknown, 'e'}
[06607]  {:unknown, '['}
[06608]  {:unknown, 'T'}
[06609]  {:unknown, 'h'}
[06610]  {:unknown, 'e'}
[06611]  {:unknown, ' '}
[06612]  {:unknown, 'c'}
[06613]  {:unknown, 'a'}
[06614]  {:unknown, 'v'}
[06615]  {:unknown, 'e'}
[06616]  {:unknown, ' '}
[06617]  {:unknown, 'i'}
[06618]  {:unknown, 's'}
[06619]  {:unknown, ' '}
[06620]  {:unknown, 's'}
[06621]  {:unknown, 'o'}
[06622]  {:unknown, 'm'}
[06623]  {:unknown, 'e'}
[06624]  {:unknown, 'w'}
[06625]  {:unknown, 'h'}
[06626]  {:unknown, 'a'}
[06627]  {:unknown, 't'}
[06628]  {:unknown, ' '}
[06629]  {:unknown, 'n'}
[06630]  {:unknown, 'a'}
[06631]  {:unknown, 'r'}
[06632]  {:unknown, 'r'}
[06633]  {:unknown, 'o'}
[06634]  {:unknown, 'w'}
[06635]  {:unknown, ' '}
[06636]  {:unknown, 'h'}
[06637]  {:unknown, 'e'}
[06638]  {:unknown, 'r'}
[06639]  {:unknown, 'e'}
[06640]  {:unknown, ','}
[06641]  {:unknown, ' '}
[06642]  {:unknown, 'a'}
[06643]  {:unknown, 'n'}
[06644]  {:unknown, 'd'}
[06645]  {:unknown, ' '}
[06646]  {:unknown, 't'}
[06647]  {:unknown, 'h'}
[06648]  {:unknown, 'e'}
[06649]  {:unknown, ' '}
[06650]  {:unknown, 'l'}
[06651]  {:unknown, 'i'}
[06652]  {:unknown, 'g'}
[06653]  {:unknown, 'h'}
[06654]  {:unknown, 't'}
[06655]  {:unknown, ' '}
[06656]  {:unknown, 'f'}
[06657]  {:unknown, 'r'}
[06658]  {:unknown, 'o'}
[06659]  {:unknown, 'm'}
[06660]  {:unknown, ' '}
[06661]  {:unknown, 't'}
[06662]  {:unknown, 'h'}
[06663]  {:unknown, 'e'}
[06664]  {:unknown, ' '}
[06665]  {:unknown, 'd'}
[06666]  {:unknown, 'o'}
[06667]  {:unknown, 'o'}
[06668]  {:unknown, 'r'}
[06669]  {:unknown, 'w'}
[06670]  {:unknown, 'a'}
[06671]  {:unknown, 'y'}
[06672]  {:unknown, ' '}
[06673]  {:unknown, 't'}
[06674]  {:unknown, 'o'}
[06675]  {:unknown, ' '}
[06676]  {:unknown, 't'}
[06677]  {:unknown, 'h'}
[06678]  {:unknown, 'e'}
[06679]  {:unknown, ' '}
[06680]  {:unknown, 's'}
[06681]  {:unknown, 'o'}
[06682]  {:unknown, 'u'}
[06683]  {:unknown, 't'}
[06684]  {:unknown, 'h'}
[06685]  {:unknown, ' '}
[06686]  {:unknown, 'i'}
[06687]  {:unknown, 's'}
[06688]  {:unknown, ' '}
[06689]  {:unknown, 'q'}
[06690]  {:unknown, 'u'}
[06691]  {:unknown, 'i'}
[06692]  {:unknown, 't'}
[06693]  {:unknown, 'e'}
[06694]  {:unknown, ' '}
[06695]  {:unknown, 'd'}
[06696]  {:unknown, 'i'}
[06697]  {:unknown, 'm'}
[06698]  {:unknown, '.'}
[06699]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[06703]  {:unknown, 't'}
[06704]  {:unknown, 'h'}
[06705]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[06709]  {:unknown, 't'}
[06710]  {:unknown, 'h'}
[06711]  {:add, {:value, 68}, {:value, 97}, {:value, 114}}
[06715]  {:unknown, 'k'}
[06716]  {:unknown, ' '}
[06717]  {:unknown, 'c'}
[06718]  {:unknown, 'a'}
[06719]  {:unknown, 'v'}
[06720]  {:unknown, 'e'}
[06721]  {:unknown, [187]}
[06722]  {:unknown, 'T'}
[06723]  {:unknown, 'h'}
[06724]  {:unknown, 'e'}
[06725]  {:unknown, ' '}
[06726]  {:unknown, 'c'}
[06727]  {:unknown, 'a'}
[06728]  {:unknown, 'v'}
[06729]  {:unknown, 'e'}
[06730]  {:unknown, ' '}
[06731]  {:unknown, 'a'}
[06732]  {:unknown, 'c'}
[06733]  {:unknown, 'o'}
[06734]  {:unknown, 'u'}
[06735]  {:unknown, 's'}
[06736]  {:unknown, 't'}
[06737]  {:unknown, 'i'}
[06738]  {:unknown, 'c'}
[06739]  {:unknown, 's'}
[06740]  {:unknown, ' '}
[06741]  {:unknown, 'd'}
[06742]  {:unknown, 'r'}
[06743]  {:unknown, 'a'}
[06744]  {:unknown, 'm'}
[06745]  {:unknown, 'a'}
[06746]  {:unknown, 't'}
[06747]  {:unknown, 'i'}
[06748]  {:unknown, 'c'}
[06749]  {:unknown, 'a'}
[06750]  {:unknown, 'l'}
[06751]  {:unknown, 'l'}
[06752]  {:unknown, 'y'}
[06753]  {:unknown, ' '}
[06754]  {:unknown, 'c'}
[06755]  {:unknown, 'h'}
[06756]  {:unknown, 'a'}
[06757]  {:unknown, 'n'}
[06758]  {:unknown, 'g'}
[06759]  {:unknown, 'e'}
[06760]  {:unknown, ' '}
[06761]  {:unknown, 'a'}
[06762]  {:unknown, 's'}
[06763]  {:unknown, ' '}
[06764]  {:unknown, 'y'}
[06765]  {:unknown, 'o'}
[06766]  {:unknown, 'u'}
[06767]  {:unknown, ' '}
[06768]  {:unknown, 'f'}
[06769]  {:unknown, 'i'}
[06770]  {:unknown, 'n'}
[06771]  {:unknown, 'd'}
[06772]  {:unknown, ' '}
[06773]  {:unknown, 'y'}
[06774]  {:unknown, 'o'}
[06775]  {:unknown, 'u'}
[06776]  {:unknown, 'r'}
[06777]  {:unknown, 's'}
[06778]  {:unknown, 'e'}
[06779]  {:unknown, 'l'}
[06780]  {:unknown, 'f'}
[06781]  {:unknown, ' '}
[06782]  {:unknown, 'a'}
[06783]  {:unknown, 't'}
[06784]  {:unknown, ' '}
[06785]  {:unknown, 'a'}
[06786]  {:unknown, ' '}
[06787]  {:unknown, 'l'}
[06788]  {:unknown, 'e'}
[06789]  {:unknown, 'g'}
[06790]  {:unknown, 'd'}
[06791]  {:unknown, 'e'}
[06792]  {:unknown, ' '}
[06793]  {:unknown, 'a'}
[06794]  {:unknown, 'b'}
[06795]  {:unknown, 'o'}
[06796]  {:unknown, 'v'}
[06797]  {:unknown, 'e'}
[06798]  {:unknown, ' '}
[06799]  {:unknown, 'a'}
[06800]  {:unknown, ' '}
[06801]  {:unknown, 'l'}
[06802]  {:unknown, 'a'}
[06803]  {:unknown, 'r'}
[06804]  {:unknown, 'g'}
[06805]  {:unknown, 'e'}
[06806]  {:unknown, ' '}
[06807]  {:unknown, 'c'}
[06808]  {:unknown, 'h'}
[06809]  {:unknown, 'a'}
[06810]  {:unknown, 's'}
[06811]  {:unknown, 'm'}
[06812]  {:unknown, '.'}
[06813]  {:unknown, ' '}
[06814]  {:unknown, ' '}
[06815]  {:unknown, 'T'}
[06816]  {:unknown, 'h'}
[06817]  {:unknown, 'e'}
[06818]  {:unknown, 'r'}
[06819]  {:unknown, 'e'}
[06820]  {:unknown, ' '}
[06821]  {:unknown, 'i'}
[06822]  {:unknown, 's'}
[06823]  {:unknown, ' '}
[06824]  {:unknown, 'b'}
[06825]  {:unknown, 'a'}
[06826]  {:unknown, 'r'}
[06827]  {:unknown, 'e'}
[06828]  {:unknown, 'l'}
[06829]  {:unknown, 'y'}
[06830]  {:unknown, ' '}
[06831]  {:unknown, 'e'}
[06832]  {:unknown, 'n'}
[06833]  {:unknown, 'o'}
[06834]  {:unknown, 'u'}
[06835]  {:unknown, 'g'}
[06836]  {:unknown, 'h'}
[06837]  {:unknown, ' '}
[06838]  {:unknown, 'l'}
[06839]  {:unknown, 'i'}
[06840]  {:unknown, 'g'}
[06841]  {:unknown, 'h'}
[06842]  {:unknown, 't'}
[06843]  {:unknown, ' '}
[06844]  {:unknown, 'h'}
[06845]  {:unknown, 'e'}
[06846]  {:unknown, 'r'}
[06847]  {:unknown, 'e'}
[06848]  {:unknown, ' '}
[06849]  {:unknown, 't'}
[06850]  {:unknown, 'o'}
[06851]  {:unknown, ' '}
[06852]  {:unknown, 'n'}
[06853]  {:unknown, 'o'}
[06854]  {:unknown, 't'}
[06855]  {:unknown, 'i'}
[06856]  {:unknown, 'c'}
[06857]  {:unknown, 'e'}
[06858]  {:unknown, ' '}
[06859]  {:unknown, 'a'}
[06860]  {:unknown, ' '}
[06861]  {:unknown, 'r'}
[06862]  {:unknown, 'o'}
[06863]  {:unknown, 'p'}
[06864]  {:unknown, 'e'}
[06865]  {:unknown, ' '}
[06866]  {:unknown, 'b'}
[06867]  {:unknown, 'r'}
[06868]  {:unknown, 'i'}
[06869]  {:unknown, 'd'}
[06870]  {:unknown, 'g'}
[06871]  {:unknown, 'e'}
[06872]  {:unknown, ' '}
[06873]  {:unknown, 'l'}
[06874]  {:unknown, 'e'}
[06875]  {:unknown, 'a'}
[06876]  {:unknown, 'd'}
[06877]  {:unknown, 'i'}
[06878]  {:unknown, 'n'}
[06879]  {:unknown, 'g'}
[06880]  {:unknown, ' '}
[06881]  {:unknown, 'o'}
[06882]  {:unknown, 'u'}
[06883]  {:unknown, 't'}
[06884]  {:unknown, ' '}
[06885]  {:unknown, 'i'}
[06886]  {:unknown, 'n'}
[06887]  {:unknown, 't'}
[06888]  {:unknown, 'o'}
[06889]  {:unknown, ' '}
[06890]  {:unknown, 't'}
[06891]  {:unknown, 'h'}
[06892]  {:unknown, 'e'}
[06893]  {:unknown, ' '}
[06894]  {:unknown, 'd'}
[06895]  {:unknown, 'a'}
[06896]  {:unknown, 'r'}
[06897]  {:unknown, 'k'}
[06898]  {:unknown, ' '}
[06899]  {:unknown, 'e'}
[06900]  {:unknown, 'm'}
[06901]  {:unknown, 'p'}
[06902]  {:unknown, 't'}
[06903]  {:unknown, 'i'}
[06904]  {:unknown, 'n'}
[06905]  {:unknown, 'e'}
[06906]  {:unknown, 's'}
[06907]  {:unknown, 's'}
[06908]  {:unknown, '.'}
[06909]  {:jmp, {:value, 98}}
[06911]  {:unknown, 'r'}
[06912]  {:unknown, 'i'}
[06913]  {:unknown, 'd'}
[06914]  {:unknown, 'g'}
[06915]  {:unknown, 'e'}
[06916]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[06920]  {:unknown, 't'}
[06921]  {:unknown, 'h'}
[06922]  {:mod, {:value, 82}, {:value, 111}, {:value, 112}}
[06926]  {:unknown, 'e'}
[06927]  {:unknown, ' '}
[06928]  {:unknown, 'b'}
[06929]  {:unknown, 'r'}
[06930]  {:unknown, 'i'}
[06931]  {:unknown, 'd'}
[06932]  {:unknown, 'g'}
[06933]  {:unknown, 'e'}
[06934]  {:unknown, 'y'}
[06935]  {:unknown, 'T'}
[06936]  {:unknown, 'h'}
[06937]  {:unknown, 'i'}
[06938]  {:unknown, 's'}
[06939]  {:unknown, ' '}
[06940]  {:unknown, 'r'}
[06941]  {:unknown, 'o'}
[06942]  {:unknown, 'p'}
[06943]  {:unknown, 'e'}
[06944]  {:unknown, ' '}
[06945]  {:unknown, 'b'}
[06946]  {:unknown, 'r'}
[06947]  {:unknown, 'i'}
[06948]  {:unknown, 'd'}
[06949]  {:unknown, 'g'}
[06950]  {:unknown, 'e'}
[06951]  {:unknown, ' '}
[06952]  {:unknown, 'c'}
[06953]  {:unknown, 'r'}
[06954]  {:unknown, 'e'}
[06955]  {:unknown, 'a'}
[06956]  {:unknown, 'k'}
[06957]  {:unknown, 's'}
[06958]  {:unknown, ' '}
[06959]  {:unknown, 'a'}
[06960]  {:unknown, 's'}
[06961]  {:unknown, ' '}
[06962]  {:unknown, 'y'}
[06963]  {:unknown, 'o'}
[06964]  {:unknown, 'u'}
[06965]  {:unknown, ' '}
[06966]  {:unknown, 'w'}
[06967]  {:unknown, 'a'}
[06968]  {:unknown, 'l'}
[06969]  {:unknown, 'k'}
[06970]  {:unknown, ' '}
[06971]  {:unknown, 'a'}
[06972]  {:unknown, 'l'}
[06973]  {:unknown, 'o'}
[06974]  {:unknown, 'n'}
[06975]  {:unknown, 'g'}
[06976]  {:unknown, ' '}
[06977]  {:unknown, 'i'}
[06978]  {:unknown, 't'}
[06979]  {:unknown, '.'}
[06980]  {:unknown, ' '}
[06981]  {:unknown, ' '}
[06982]  {:unknown, 'Y'}
[06983]  {:unknown, 'o'}
[06984]  {:unknown, 'u'}
[06985]  {:unknown, ' '}
[06986]  {:unknown, 'a'}
[06987]  {:unknown, 'r'}
[06988]  {:unknown, 'e'}
[06989]  {:unknown, 'n'}
[06990]  {:unknown, '\''}
[06991]  {:unknown, 't'}
[06992]  {:unknown, ' '}
[06993]  {:unknown, 's'}
[06994]  {:unknown, 'u'}
[06995]  {:unknown, 'r'}
[06996]  {:unknown, 'e'}
[06997]  {:unknown, ' '}
[06998]  {:unknown, 'h'}
[06999]  {:unknown, 'o'}
[07000]  {:unknown, 'w'}
[07001]  {:unknown, ' '}
[07002]  {:unknown, 'o'}
[07003]  {:unknown, 'l'}
[07004]  {:unknown, 'd'}
[07005]  {:unknown, ' '}
[07006]  {:unknown, 'i'}
[07007]  {:unknown, 't'}
[07008]  {:unknown, ' '}
[07009]  {:unknown, 'i'}
[07010]  {:unknown, 's'}
[07011]  {:unknown, ','}
[07012]  {:unknown, ' '}
[07013]  {:unknown, 'o'}
[07014]  {:unknown, 'r'}
[07015]  {:unknown, ' '}
[07016]  {:unknown, 'w'}
[07017]  {:unknown, 'h'}
[07018]  {:unknown, 'e'}
[07019]  {:unknown, 't'}
[07020]  {:unknown, 'h'}
[07021]  {:unknown, 'e'}
[07022]  {:unknown, 'r'}
[07023]  {:unknown, ' '}
[07024]  {:unknown, 'i'}
[07025]  {:unknown, 't'}
[07026]  {:unknown, ' '}
[07027]  {:unknown, 'c'}
[07028]  {:unknown, 'a'}
[07029]  {:unknown, 'n'}
[07030]  {:unknown, ' '}
[07031]  {:unknown, 'e'}
[07032]  {:unknown, 'v'}
[07033]  {:unknown, 'e'}
[07034]  {:unknown, 'n'}
[07035]  {:unknown, ' '}
[07036]  {:unknown, 's'}
[07037]  {:unknown, 'u'}
[07038]  {:unknown, 'p'}
[07039]  {:unknown, 'p'}
[07040]  {:unknown, 'o'}
[07041]  {:unknown, 'r'}
[07042]  {:unknown, 't'}
[07043]  {:unknown, ' '}
[07044]  {:unknown, 'y'}
[07045]  {:unknown, 'o'}
[07046]  {:unknown, 'u'}
[07047]  {:unknown, 'r'}
[07048]  {:unknown, ' '}
[07049]  {:unknown, 'w'}
[07050]  {:unknown, 'e'}
[07051]  {:unknown, 'i'}
[07052]  {:unknown, 'g'}
[07053]  {:unknown, 'h'}
[07054]  {:unknown, 't'}
[07055]  {:unknown, '.'}
[07056]  {:jf, {:value, 99}, {:value, 111}}
[07059]  {:unknown, 'n'}
[07060]  {:unknown, 't'}
[07061]  {:unknown, 'i'}
[07062]  {:unknown, 'n'}
[07063]  {:unknown, 'u'}
[07064]  {:unknown, 'e'}
[07065]  {:eq, {:value, 98}, {:value, 97}, {:value, 99}}
[07069]  {:unknown, 'k'}
[07070]  {:unknown, [24]}
[07071]  {:unknown, 'F'}
[07072]  {:unknown, 'a'}
[07073]  {:unknown, 'l'}
[07074]  {:unknown, 'l'}
[07075]  {:unknown, 'i'}
[07076]  {:unknown, 'n'}
[07077]  {:unknown, 'g'}
[07078]  {:unknown, ' '}
[07079]  {:unknown, 't'}
[07080]  {:unknown, 'h'}
[07081]  {:unknown, 'r'}
[07082]  {:unknown, 'o'}
[07083]  {:unknown, 'u'}
[07084]  {:unknown, 'g'}
[07085]  {:unknown, 'h'}
[07086]  {:unknown, ' '}
[07087]  {:unknown, 't'}
[07088]  {:unknown, 'h'}
[07089]  {:unknown, 'e'}
[07090]  {:unknown, ' '}
[07091]  {:unknown, 'a'}
[07092]  {:unknown, 'i'}
[07093]  {:unknown, 'r'}
[07094]  {:unknown, '!'}
[07095]  {:unknown, [169]}
[07096]  {:unknown, 'A'}
[07097]  {:unknown, 's'}
[07098]  {:unknown, ' '}
[07099]  {:unknown, 'y'}
[07100]  {:unknown, 'o'}
[07101]  {:unknown, 'u'}
[07102]  {:unknown, ' '}
[07103]  {:unknown, 'c'}
[07104]  {:unknown, 'o'}
[07105]  {:unknown, 'n'}
[07106]  {:unknown, 't'}
[07107]  {:unknown, 'i'}
[07108]  {:unknown, 'n'}
[07109]  {:unknown, 'u'}
[07110]  {:unknown, 'e'}
[07111]  {:unknown, ' '}
[07112]  {:unknown, 'a'}
[07113]  {:unknown, 'l'}
[07114]  {:unknown, 'o'}
[07115]  {:unknown, 'n'}
[07116]  {:unknown, 'g'}
[07117]  {:unknown, ' '}
[07118]  {:unknown, 't'}
[07119]  {:unknown, 'h'}
[07120]  {:unknown, 'e'}
[07121]  {:unknown, ' '}
[07122]  {:unknown, 'b'}
[07123]  {:unknown, 'r'}
[07124]  {:unknown, 'i'}
[07125]  {:unknown, 'd'}
[07126]  {:unknown, 'g'}
[07127]  {:unknown, 'e'}
[07128]  {:unknown, ','}
[07129]  {:unknown, ' '}
[07130]  {:unknown, 'i'}
[07131]  {:unknown, 't'}
[07132]  {:unknown, ' '}
[07133]  {:unknown, 's'}
[07134]  {:unknown, 'n'}
[07135]  {:unknown, 'a'}
[07136]  {:unknown, 'p'}
[07137]  {:unknown, 's'}
[07138]  {:unknown, '!'}
[07139]  {:unknown, ' '}
[07140]  {:unknown, ' '}
[07141]  {:unknown, 'Y'}
[07142]  {:unknown, 'o'}
[07143]  {:unknown, 'u'}
[07144]  {:unknown, ' '}
[07145]  {:unknown, 't'}
[07146]  {:unknown, 'r'}
[07147]  {:unknown, 'y'}
[07148]  {:unknown, ' '}
[07149]  {:unknown, 't'}
[07150]  {:unknown, 'o'}
[07151]  {:unknown, ' '}
[07152]  {:unknown, 'g'}
[07153]  {:unknown, 'r'}
[07154]  {:unknown, 'a'}
[07155]  {:unknown, 'b'}
[07156]  {:unknown, ' '}
[07157]  {:unknown, 't'}
[07158]  {:unknown, 'h'}
[07159]  {:unknown, 'e'}
[07160]  {:unknown, ' '}
[07161]  {:unknown, 'b'}
[07162]  {:unknown, 'r'}
[07163]  {:unknown, 'i'}
[07164]  {:unknown, 'd'}
[07165]  {:unknown, 'g'}
[07166]  {:unknown, 'e'}
[07167]  {:unknown, ','}
[07168]  {:unknown, ' '}
[07169]  {:unknown, 'b'}
[07170]  {:unknown, 'u'}
[07171]  {:unknown, 't'}
[07172]  {:unknown, ' '}
[07173]  {:unknown, 'i'}
[07174]  {:unknown, 't'}
[07175]  {:unknown, ' '}
[07176]  {:unknown, 'e'}
[07177]  {:unknown, 'v'}
[07178]  {:unknown, 'a'}
[07179]  {:unknown, 'd'}
[07180]  {:unknown, 'e'}
[07181]  {:unknown, 's'}
[07182]  {:unknown, ' '}
[07183]  {:unknown, 'y'}
[07184]  {:unknown, 'o'}
[07185]  {:unknown, 'u'}
[07186]  {:unknown, 'r'}
[07187]  {:unknown, ' '}
[07188]  {:unknown, 'g'}
[07189]  {:unknown, 'r'}
[07190]  {:unknown, 'a'}
[07191]  {:unknown, 's'}
[07192]  {:unknown, 'p'}
[07193]  {:unknown, ' '}
[07194]  {:unknown, 'i'}
[07195]  {:unknown, 'n'}
[07196]  {:unknown, ' '}
[07197]  {:unknown, 't'}
[07198]  {:unknown, 'h'}
[07199]  {:unknown, 'e'}
[07200]  {:unknown, ' '}
[07201]  {:unknown, 'd'}
[07202]  {:unknown, 'a'}
[07203]  {:unknown, 'r'}
[07204]  {:unknown, 'k'}
[07205]  {:unknown, 'n'}
[07206]  {:unknown, 'e'}
[07207]  {:unknown, 's'}
[07208]  {:unknown, 's'}
[07209]  {:unknown, '.'}
[07210]  {:unknown, ' '}
[07211]  {:unknown, ' '}
[07212]  {:unknown, 'Y'}
[07213]  {:unknown, 'o'}
[07214]  {:unknown, 'u'}
[07215]  {:unknown, ' '}
[07216]  {:unknown, 'a'}
[07217]  {:unknown, 'r'}
[07218]  {:unknown, 'e'}
[07219]  {:unknown, ' '}
[07220]  {:unknown, 'p'}
[07221]  {:unknown, 'l'}
[07222]  {:unknown, 'u'}
[07223]  {:unknown, 'm'}
[07224]  {:unknown, 'm'}
[07225]  {:unknown, 'e'}
[07226]  {:unknown, 't'}
[07227]  {:unknown, 'i'}
[07228]  {:unknown, 'n'}
[07229]  {:unknown, 'g'}
[07230]  {:unknown, ' '}
[07231]  {:unknown, 'q'}
[07232]  {:unknown, 'u'}
[07233]  {:unknown, 'i'}
[07234]  {:unknown, 'c'}
[07235]  {:unknown, 'k'}
[07236]  {:unknown, 'l'}
[07237]  {:unknown, 'y'}
[07238]  {:unknown, ' '}
[07239]  {:unknown, 'd'}
[07240]  {:unknown, 'o'}
[07241]  {:unknown, 'w'}
[07242]  {:unknown, 'n'}
[07243]  {:unknown, 'w'}
[07244]  {:unknown, 'a'}
[07245]  {:unknown, 'r'}
[07246]  {:unknown, 'd'}
[07247]  {:unknown, ' '}
[07248]  {:unknown, 'i'}
[07249]  {:unknown, 'n'}
[07250]  {:unknown, 't'}
[07251]  {:unknown, 'o'}
[07252]  {:unknown, ' '}
[07253]  {:unknown, 't'}
[07254]  {:unknown, 'h'}
[07255]  {:unknown, 'e'}
[07256]  {:unknown, ' '}
[07257]  {:unknown, 'c'}
[07258]  {:unknown, 'h'}
[07259]  {:unknown, 'a'}
[07260]  {:unknown, 's'}
[07261]  {:unknown, 'm'}
[07262]  {:unknown, '.'}
[07263]  {:unknown, '.'}
[07264]  {:unknown, '.'}
[07265]  {:eq, {:value, 100}, {:value, 111}, {:value, 119}}
[07269]  {:unknown, 'n'}
[07270]  {:mod, {:value, 77}, {:value, 111}, {:value, 115}}
[07274]  {:unknown, 's'}
[07275]  {:unknown, ' '}
[07276]  {:unknown, 'c'}
[07277]  {:unknown, 'a'}
[07278]  {:unknown, 'v'}
[07279]  {:unknown, 'e'}
[07280]  {:unknown, 'r'}
[07281]  {:unknown, 'n'}
[07282]  {:unknown, [209]}
[07283]  {:unknown, 'Y'}
[07284]  {:unknown, 'o'}
[07285]  {:unknown, 'u'}
[07286]  {:unknown, ' '}
[07287]  {:unknown, 'a'}
[07288]  {:unknown, 'r'}
[07289]  {:unknown, 'e'}
[07290]  {:unknown, ' '}
[07291]  {:unknown, 's'}
[07292]  {:unknown, 't'}
[07293]  {:unknown, 'a'}
[07294]  {:unknown, 'n'}
[07295]  {:unknown, 'd'}
[07296]  {:unknown, 'i'}
[07297]  {:unknown, 'n'}
[07298]  {:unknown, 'g'}
[07299]  {:unknown, ' '}
[07300]  {:unknown, 'i'}
[07301]  {:unknown, 'n'}
[07302]  {:unknown, ' '}
[07303]  {:unknown, 'a'}
[07304]  {:unknown, ' '}
[07305]  {:unknown, 'l'}
[07306]  {:unknown, 'a'}
[07307]  {:unknown, 'r'}
[07308]  {:unknown, 'g'}
[07309]  {:unknown, 'e'}
[07310]  {:unknown, ' '}
[07311]  {:unknown, 'c'}
[07312]  {:unknown, 'a'}
[07313]  {:unknown, 'v'}
[07314]  {:unknown, 'e'}
[07315]  {:unknown, 'r'}
[07316]  {:unknown, 'n'}
[07317]  {:unknown, ' '}
[07318]  {:unknown, 'f'}
[07319]  {:unknown, 'u'}
[07320]  {:unknown, 'l'}
[07321]  {:unknown, 'l'}
[07322]  {:unknown, ' '}
[07323]  {:unknown, 'o'}
[07324]  {:unknown, 'f'}
[07325]  {:unknown, ' '}
[07326]  {:unknown, 'b'}
[07327]  {:unknown, 'i'}
[07328]  {:unknown, 'o'}
[07329]  {:unknown, 'l'}
[07330]  {:unknown, 'u'}
[07331]  {:unknown, 'm'}
[07332]  {:unknown, 'i'}
[07333]  {:unknown, 'n'}
[07334]  {:unknown, 'e'}
[07335]  {:unknown, 's'}
[07336]  {:unknown, 'c'}
[07337]  {:unknown, 'e'}
[07338]  {:unknown, 'n'}
[07339]  {:unknown, 't'}
[07340]  {:unknown, ' '}
[07341]  {:unknown, 'm'}
[07342]  {:unknown, 'o'}
[07343]  {:unknown, 's'}
[07344]  {:unknown, 's'}
[07345]  {:unknown, '.'}
[07346]  {:unknown, ' '}
[07347]  {:unknown, ' '}
[07348]  {:unknown, 'I'}
[07349]  {:unknown, 't'}
[07350]  {:unknown, ' '}
[07351]  {:unknown, 'm'}
[07352]  {:unknown, 'u'}
[07353]  {:unknown, 's'}
[07354]  {:unknown, 't'}
[07355]  {:unknown, ' '}
[07356]  {:unknown, 'h'}
[07357]  {:unknown, 'a'}
[07358]  {:unknown, 'v'}
[07359]  {:unknown, 'e'}
[07360]  {:unknown, ' '}
[07361]  {:unknown, 'b'}
[07362]  {:unknown, 'r'}
[07363]  {:unknown, 'o'}
[07364]  {:unknown, 'k'}
[07365]  {:unknown, 'e'}
[07366]  {:unknown, 'n'}
[07367]  {:unknown, ' '}
[07368]  {:unknown, 'y'}
[07369]  {:unknown, 'o'}
[07370]  {:unknown, 'u'}
[07371]  {:unknown, 'r'}
[07372]  {:unknown, ' '}
[07373]  {:unknown, 'f'}
[07374]  {:unknown, 'a'}
[07375]  {:unknown, 'l'}
[07376]  {:unknown, 'l'}
[07377]  {:unknown, '!'}
[07378]  {:unknown, ' '}
[07379]  {:unknown, ' '}
[07380]  {:unknown, 'T'}
[07381]  {:unknown, 'h'}
[07382]  {:unknown, 'e'}
[07383]  {:unknown, ' '}
[07384]  {:unknown, 'c'}
[07385]  {:unknown, 'a'}
[07386]  {:unknown, 'v'}
[07387]  {:unknown, 'e'}
[07388]  {:unknown, 'r'}
[07389]  {:unknown, 'n'}
[07390]  {:unknown, ' '}
[07391]  {:unknown, 'e'}
[07392]  {:unknown, 'x'}
[07393]  {:unknown, 't'}
[07394]  {:unknown, 'e'}
[07395]  {:unknown, 'n'}
[07396]  {:unknown, 'd'}
[07397]  {:unknown, 's'}
[07398]  {:unknown, ' '}
[07399]  {:unknown, 't'}
[07400]  {:unknown, 'o'}
[07401]  {:unknown, ' '}
[07402]  {:unknown, 't'}
[07403]  {:unknown, 'h'}
[07404]  {:unknown, 'e'}
[07405]  {:unknown, ' '}
[07406]  {:unknown, 'e'}
[07407]  {:unknown, 'a'}
[07408]  {:unknown, 's'}
[07409]  {:unknown, 't'}
[07410]  {:unknown, ' '}
[07411]  {:unknown, 'a'}
[07412]  {:unknown, 'n'}
[07413]  {:unknown, 'd'}
[07414]  {:unknown, ' '}
[07415]  {:unknown, 'w'}
[07416]  {:unknown, 'e'}
[07417]  {:unknown, 's'}
[07418]  {:unknown, 't'}
[07419]  {:unknown, ';'}
[07420]  {:unknown, ' '}
[07421]  {:unknown, 'a'}
[07422]  {:unknown, 't'}
[07423]  {:unknown, ' '}
[07424]  {:unknown, 't'}
[07425]  {:unknown, 'h'}
[07426]  {:unknown, 'e'}
[07427]  {:unknown, ' '}
[07428]  {:unknown, 'w'}
[07429]  {:unknown, 'e'}
[07430]  {:unknown, 's'}
[07431]  {:unknown, 't'}
[07432]  {:unknown, ' '}
[07433]  {:unknown, 'e'}
[07434]  {:unknown, 'n'}
[07435]  {:unknown, 'd'}
[07436]  {:unknown, ','}
[07437]  {:unknown, ' '}
[07438]  {:unknown, 'y'}
[07439]  {:unknown, 'o'}
[07440]  {:unknown, 'u'}
[07441]  {:unknown, ' '}
[07442]  {:unknown, 't'}
[07443]  {:unknown, 'h'}
[07444]  {:unknown, 'i'}
[07445]  {:unknown, 'n'}
[07446]  {:unknown, 'k'}
[07447]  {:unknown, ' '}
[07448]  {:unknown, 'y'}
[07449]  {:unknown, 'o'}
[07450]  {:unknown, 'u'}
[07451]  {:unknown, ' '}
[07452]  {:unknown, 's'}
[07453]  {:unknown, 'e'}
[07454]  {:unknown, 'e'}
[07455]  {:unknown, ' '}
[07456]  {:unknown, 'a'}
[07457]  {:unknown, ' '}
[07458]  {:unknown, 'p'}
[07459]  {:unknown, 'a'}
[07460]  {:unknown, 's'}
[07461]  {:unknown, 's'}
[07462]  {:unknown, 'a'}
[07463]  {:unknown, 'g'}
[07464]  {:unknown, 'e'}
[07465]  {:unknown, ' '}
[07466]  {:unknown, 'l'}
[07467]  {:unknown, 'e'}
[07468]  {:unknown, 'a'}
[07469]  {:unknown, 'd'}
[07470]  {:unknown, 'i'}
[07471]  {:unknown, 'n'}
[07472]  {:unknown, 'g'}
[07473]  {:unknown, ' '}
[07474]  {:unknown, 'o'}
[07475]  {:unknown, 'u'}
[07476]  {:unknown, 't'}
[07477]  {:unknown, ' '}
[07478]  {:unknown, 'o'}
[07479]  {:unknown, 'f'}
[07480]  {:unknown, ' '}
[07481]  {:unknown, 't'}
[07482]  {:unknown, 'h'}
[07483]  {:unknown, 'e'}
[07484]  {:unknown, ' '}
[07485]  {:unknown, 'c'}
[07486]  {:unknown, 'a'}
[07487]  {:unknown, 'v'}
[07488]  {:unknown, 'e'}
[07489]  {:unknown, 'r'}
[07490]  {:unknown, 'n'}
[07491]  {:unknown, '.'}
[07492]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[07496]  {:unknown, 't'}
[07497]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[07501]  {:unknown, 't'}
[07502]  {:mod, {:value, 77}, {:value, 111}, {:value, 115}}
[07506]  {:unknown, 's'}
[07507]  {:unknown, ' '}
[07508]  {:unknown, 'c'}
[07509]  {:unknown, 'a'}
[07510]  {:unknown, 'v'}
[07511]  {:unknown, 'e'}
[07512]  {:unknown, 'r'}
[07513]  {:unknown, 'n'}
[07514]  {:unknown, '`'}
[07515]  {:unknown, 'Y'}
[07516]  {:unknown, 'o'}
[07517]  {:unknown, 'u'}
[07518]  {:unknown, ' '}
[07519]  {:unknown, 'a'}
[07520]  {:unknown, 'r'}
[07521]  {:unknown, 'e'}
[07522]  {:unknown, ' '}
[07523]  {:unknown, 's'}
[07524]  {:unknown, 't'}
[07525]  {:unknown, 'a'}
[07526]  {:unknown, 'n'}
[07527]  {:unknown, 'd'}
[07528]  {:unknown, 'i'}
[07529]  {:unknown, 'n'}
[07530]  {:unknown, 'g'}
[07531]  {:unknown, ' '}
[07532]  {:unknown, 'i'}
[07533]  {:unknown, 'n'}
[07534]  {:unknown, ' '}
[07535]  {:unknown, 'a'}
[07536]  {:unknown, ' '}
[07537]  {:unknown, 'l'}
[07538]  {:unknown, 'a'}
[07539]  {:unknown, 'r'}
[07540]  {:unknown, 'g'}
[07541]  {:unknown, 'e'}
[07542]  {:unknown, ' '}
[07543]  {:unknown, 'c'}
[07544]  {:unknown, 'a'}
[07545]  {:unknown, 'v'}
[07546]  {:unknown, 'e'}
[07547]  {:unknown, 'r'}
[07548]  {:unknown, 'n'}
[07549]  {:unknown, ' '}
[07550]  {:unknown, 'f'}
[07551]  {:unknown, 'u'}
[07552]  {:unknown, 'l'}
[07553]  {:unknown, 'l'}
[07554]  {:unknown, ' '}
[07555]  {:unknown, 'o'}
[07556]  {:unknown, 'f'}
[07557]  {:unknown, ' '}
[07558]  {:unknown, 'b'}
[07559]  {:unknown, 'i'}
[07560]  {:unknown, 'o'}
[07561]  {:unknown, 'l'}
[07562]  {:unknown, 'u'}
[07563]  {:unknown, 'm'}
[07564]  {:unknown, 'i'}
[07565]  {:unknown, 'n'}
[07566]  {:unknown, 'e'}
[07567]  {:unknown, 's'}
[07568]  {:unknown, 'c'}
[07569]  {:unknown, 'e'}
[07570]  {:unknown, 'n'}
[07571]  {:unknown, 't'}
[07572]  {:unknown, ' '}
[07573]  {:unknown, 'm'}
[07574]  {:unknown, 'o'}
[07575]  {:unknown, 's'}
[07576]  {:unknown, 's'}
[07577]  {:unknown, '.'}
[07578]  {:unknown, ' '}
[07579]  {:unknown, ' '}
[07580]  {:unknown, 'T'}
[07581]  {:unknown, 'h'}
[07582]  {:unknown, 'e'}
[07583]  {:unknown, ' '}
[07584]  {:unknown, 'c'}
[07585]  {:unknown, 'a'}
[07586]  {:unknown, 'v'}
[07587]  {:unknown, 'e'}
[07588]  {:unknown, 'r'}
[07589]  {:unknown, 'n'}
[07590]  {:unknown, ' '}
[07591]  {:unknown, 'e'}
[07592]  {:unknown, 'x'}
[07593]  {:unknown, 't'}
[07594]  {:unknown, 'e'}
[07595]  {:unknown, 'n'}
[07596]  {:unknown, 'd'}
[07597]  {:unknown, 's'}
[07598]  {:unknown, ' '}
[07599]  {:unknown, 't'}
[07600]  {:unknown, 'o'}
[07601]  {:unknown, ' '}
[07602]  {:unknown, 't'}
[07603]  {:unknown, 'h'}
[07604]  {:unknown, 'e'}
[07605]  {:unknown, ' '}
[07606]  {:unknown, 'w'}
[07607]  {:unknown, 'e'}
[07608]  {:unknown, 's'}
[07609]  {:unknown, 't'}
[07610]  {:unknown, '.'}
[07611]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[07615]  {:unknown, 't'}
[07616]  {:mod, {:value, 77}, {:value, 111}, {:value, 115}}
[07620]  {:unknown, 's'}
[07621]  {:unknown, ' '}
[07622]  {:unknown, 'c'}
[07623]  {:unknown, 'a'}
[07624]  {:unknown, 'v'}
[07625]  {:unknown, 'e'}
[07626]  {:unknown, 'r'}
[07627]  {:unknown, 'n'}
[07628]  {:unknown, [157]}
[07629]  {:unknown, 'Y'}
[07630]  {:unknown, 'o'}
[07631]  {:unknown, 'u'}
[07632]  {:unknown, ' '}
[07633]  {:unknown, 'a'}
[07634]  {:unknown, 'r'}
[07635]  {:unknown, 'e'}
[07636]  {:unknown, ' '}
[07637]  {:unknown, 's'}
[07638]  {:unknown, 't'}
[07639]  {:unknown, 'a'}
[07640]  {:unknown, 'n'}
[07641]  {:unknown, 'd'}
[07642]  {:unknown, 'i'}
[07643]  {:unknown, 'n'}
[07644]  {:unknown, 'g'}
[07645]  {:unknown, ' '}
[07646]  {:unknown, 'i'}
[07647]  {:unknown, 'n'}
[07648]  {:unknown, ' '}
[07649]  {:unknown, 'a'}
[07650]  {:unknown, ' '}
[07651]  {:unknown, 'l'}
[07652]  {:unknown, 'a'}
[07653]  {:unknown, 'r'}
[07654]  {:unknown, 'g'}
[07655]  {:unknown, 'e'}
[07656]  {:unknown, ' '}
[07657]  {:unknown, 'c'}
[07658]  {:unknown, 'a'}
[07659]  {:unknown, 'v'}
[07660]  {:unknown, 'e'}
[07661]  {:unknown, 'r'}
[07662]  {:unknown, 'n'}
[07663]  {:unknown, ' '}
[07664]  {:unknown, 'f'}
[07665]  {:unknown, 'u'}
[07666]  {:unknown, 'l'}
[07667]  {:unknown, 'l'}
[07668]  {:unknown, ' '}
[07669]  {:unknown, 'o'}
[07670]  {:unknown, 'f'}
[07671]  {:unknown, ' '}
[07672]  {:unknown, 'b'}
[07673]  {:unknown, 'i'}
[07674]  {:unknown, 'o'}
[07675]  {:unknown, 'l'}
[07676]  {:unknown, 'u'}
[07677]  {:unknown, 'm'}
[07678]  {:unknown, 'i'}
[07679]  {:unknown, 'n'}
[07680]  {:unknown, 'e'}
[07681]  {:unknown, 's'}
[07682]  {:unknown, 'c'}
[07683]  {:unknown, 'e'}
[07684]  {:unknown, 'n'}
[07685]  {:unknown, 't'}
[07686]  {:unknown, ' '}
[07687]  {:unknown, 'm'}
[07688]  {:unknown, 'o'}
[07689]  {:unknown, 's'}
[07690]  {:unknown, 's'}
[07691]  {:unknown, '.'}
[07692]  {:unknown, ' '}
[07693]  {:unknown, ' '}
[07694]  {:unknown, 'T'}
[07695]  {:unknown, 'h'}
[07696]  {:unknown, 'e'}
[07697]  {:unknown, ' '}
[07698]  {:unknown, 'c'}
[07699]  {:unknown, 'a'}
[07700]  {:unknown, 'v'}
[07701]  {:unknown, 'e'}
[07702]  {:unknown, 'r'}
[07703]  {:unknown, 'n'}
[07704]  {:unknown, ' '}
[07705]  {:unknown, 'e'}
[07706]  {:unknown, 'x'}
[07707]  {:unknown, 't'}
[07708]  {:unknown, 'e'}
[07709]  {:unknown, 'n'}
[07710]  {:unknown, 'd'}
[07711]  {:unknown, 's'}
[07712]  {:unknown, ' '}
[07713]  {:unknown, 't'}
[07714]  {:unknown, 'o'}
[07715]  {:unknown, ' '}
[07716]  {:unknown, 't'}
[07717]  {:unknown, 'h'}
[07718]  {:unknown, 'e'}
[07719]  {:unknown, ' '}
[07720]  {:unknown, 'e'}
[07721]  {:unknown, 'a'}
[07722]  {:unknown, 's'}
[07723]  {:unknown, 't'}
[07724]  {:unknown, '.'}
[07725]  {:unknown, ' '}
[07726]  {:unknown, ' '}
[07727]  {:unknown, 'T'}
[07728]  {:unknown, 'h'}
[07729]  {:unknown, 'e'}
[07730]  {:unknown, 'r'}
[07731]  {:unknown, 'e'}
[07732]  {:unknown, ' '}
[07733]  {:unknown, 'i'}
[07734]  {:unknown, 's'}
[07735]  {:unknown, ' '}
[07736]  {:unknown, 'a'}
[07737]  {:unknown, ' '}
[07738]  {:unknown, 'c'}
[07739]  {:unknown, 'r'}
[07740]  {:unknown, 'e'}
[07741]  {:unknown, 'v'}
[07742]  {:unknown, 'i'}
[07743]  {:unknown, 's'}
[07744]  {:unknown, 'e'}
[07745]  {:unknown, ' '}
[07746]  {:unknown, 'i'}
[07747]  {:unknown, 'n'}
[07748]  {:unknown, ' '}
[07749]  {:unknown, 't'}
[07750]  {:unknown, 'h'}
[07751]  {:unknown, 'e'}
[07752]  {:unknown, ' '}
[07753]  {:unknown, 'r'}
[07754]  {:unknown, 'o'}
[07755]  {:unknown, 'c'}
[07756]  {:unknown, 'k'}
[07757]  {:unknown, 's'}
[07758]  {:unknown, ' '}
[07759]  {:unknown, 'w'}
[07760]  {:unknown, 'h'}
[07761]  {:unknown, 'i'}
[07762]  {:unknown, 'c'}
[07763]  {:unknown, 'h'}
[07764]  {:unknown, ' '}
[07765]  {:unknown, 'o'}
[07766]  {:unknown, 'p'}
[07767]  {:unknown, 'e'}
[07768]  {:unknown, 'n'}
[07769]  {:unknown, 's'}
[07770]  {:unknown, ' '}
[07771]  {:unknown, 'i'}
[07772]  {:unknown, 'n'}
[07773]  {:unknown, 't'}
[07774]  {:unknown, 'o'}
[07775]  {:unknown, ' '}
[07776]  {:unknown, 'a'}
[07777]  {:unknown, ' '}
[07778]  {:unknown, 'p'}
[07779]  {:unknown, 'a'}
[07780]  {:unknown, 's'}
[07781]  {:unknown, 's'}
[07782]  {:unknown, 'a'}
[07783]  {:unknown, 'g'}
[07784]  {:unknown, 'e'}
[07785]  {:unknown, '.'}
[07786]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[07790]  {:unknown, 't'}
[07791]  {:jt, {:value, 112}, {:value, 97}}
[07794]  {:unknown, 's'}
[07795]  {:unknown, 's'}
[07796]  {:unknown, 'a'}
[07797]  {:unknown, 'g'}
[07798]  {:unknown, 'e'}
[07799]  {:jt, {:value, 80}, {:value, 97}}
[07802]  {:unknown, 's'}
[07803]  {:unknown, 's'}
[07804]  {:unknown, 'a'}
[07805]  {:unknown, 'g'}
[07806]  {:unknown, 'e'}
[07807]  {:unknown, [176]}
[07808]  {:unknown, 'Y'}
[07809]  {:unknown, 'o'}
[07810]  {:unknown, 'u'}
[07811]  {:unknown, ' '}
[07812]  {:unknown, 'a'}
[07813]  {:unknown, 'r'}
[07814]  {:unknown, 'e'}
[07815]  {:unknown, ' '}
[07816]  {:unknown, 'i'}
[07817]  {:unknown, 'n'}
[07818]  {:unknown, ' '}
[07819]  {:unknown, 'a'}
[07820]  {:unknown, ' '}
[07821]  {:unknown, 'c'}
[07822]  {:unknown, 'r'}
[07823]  {:unknown, 'e'}
[07824]  {:unknown, 'v'}
[07825]  {:unknown, 'i'}
[07826]  {:unknown, 's'}
[07827]  {:unknown, 'e'}
[07828]  {:unknown, ' '}
[07829]  {:unknown, 'o'}
[07830]  {:unknown, 'n'}
[07831]  {:unknown, ' '}
[07832]  {:unknown, 't'}
[07833]  {:unknown, 'h'}
[07834]  {:unknown, 'e'}
[07835]  {:unknown, ' '}
[07836]  {:unknown, 'w'}
[07837]  {:unknown, 'e'}
[07838]  {:unknown, 's'}
[07839]  {:unknown, 't'}
[07840]  {:unknown, ' '}
[07841]  {:unknown, 'w'}
[07842]  {:unknown, 'a'}
[07843]  {:unknown, 'l'}
[07844]  {:unknown, 'l'}
[07845]  {:unknown, ' '}
[07846]  {:unknown, 'o'}
[07847]  {:unknown, 'f'}
[07848]  {:unknown, ' '}
[07849]  {:unknown, 't'}
[07850]  {:unknown, 'h'}
[07851]  {:unknown, 'e'}
[07852]  {:unknown, ' '}
[07853]  {:unknown, 'm'}
[07854]  {:unknown, 'o'}
[07855]  {:unknown, 's'}
[07856]  {:unknown, 's'}
[07857]  {:unknown, ' '}
[07858]  {:unknown, 'c'}
[07859]  {:unknown, 'a'}
[07860]  {:unknown, 'v'}
[07861]  {:unknown, 'e'}
[07862]  {:unknown, 'r'}
[07863]  {:unknown, 'n'}
[07864]  {:unknown, '.'}
[07865]  {:unknown, ' '}
[07866]  {:unknown, ' '}
[07867]  {:unknown, 'A'}
[07868]  {:unknown, ' '}
[07869]  {:unknown, 'd'}
[07870]  {:unknown, 'a'}
[07871]  {:unknown, 'r'}
[07872]  {:unknown, 'k'}
[07873]  {:unknown, ' '}
[07874]  {:unknown, 'p'}
[07875]  {:unknown, 'a'}
[07876]  {:unknown, 's'}
[07877]  {:unknown, 's'}
[07878]  {:unknown, 'a'}
[07879]  {:unknown, 'g'}
[07880]  {:unknown, 'e'}
[07881]  {:unknown, ' '}
[07882]  {:unknown, 'l'}
[07883]  {:unknown, 'e'}
[07884]  {:unknown, 'a'}
[07885]  {:unknown, 'd'}
[07886]  {:unknown, 's'}
[07887]  {:unknown, ' '}
[07888]  {:unknown, 'f'}
[07889]  {:unknown, 'u'}
[07890]  {:unknown, 'r'}
[07891]  {:unknown, 't'}
[07892]  {:unknown, 'h'}
[07893]  {:unknown, 'e'}
[07894]  {:unknown, 'r'}
[07895]  {:unknown, ' '}
[07896]  {:unknown, 'w'}
[07897]  {:unknown, 'e'}
[07898]  {:unknown, 's'}
[07899]  {:unknown, 't'}
[07900]  {:unknown, '.'}
[07901]  {:unknown, ' '}
[07902]  {:unknown, ' '}
[07903]  {:unknown, 'T'}
[07904]  {:unknown, 'h'}
[07905]  {:unknown, 'e'}
[07906]  {:unknown, 'r'}
[07907]  {:unknown, 'e'}
[07908]  {:unknown, ' '}
[07909]  {:unknown, 'i'}
[07910]  {:unknown, 's'}
[07911]  {:unknown, ' '}
[07912]  {:unknown, 'a'}
[07913]  {:unknown, ' '}
[07914]  {:unknown, 'l'}
[07915]  {:unknown, 'a'}
[07916]  {:unknown, 'd'}
[07917]  {:unknown, 'd'}
[07918]  {:unknown, 'e'}
[07919]  {:unknown, 'r'}
[07920]  {:unknown, ' '}
[07921]  {:unknown, 'h'}
[07922]  {:unknown, 'e'}
[07923]  {:unknown, 'r'}
[07924]  {:unknown, 'e'}
[07925]  {:unknown, ' '}
[07926]  {:unknown, 'w'}
[07927]  {:unknown, 'h'}
[07928]  {:unknown, 'i'}
[07929]  {:unknown, 'c'}
[07930]  {:unknown, 'h'}
[07931]  {:unknown, ' '}
[07932]  {:unknown, 'l'}
[07933]  {:unknown, 'e'}
[07934]  {:unknown, 'a'}
[07935]  {:unknown, 'd'}
[07936]  {:unknown, 's'}
[07937]  {:unknown, ' '}
[07938]  {:unknown, 'd'}
[07939]  {:unknown, 'o'}
[07940]  {:unknown, 'w'}
[07941]  {:unknown, 'n'}
[07942]  {:unknown, ' '}
[07943]  {:unknown, 'i'}
[07944]  {:unknown, 'n'}
[07945]  {:unknown, 't'}
[07946]  {:unknown, 'o'}
[07947]  {:unknown, ' '}
[07948]  {:unknown, 'a'}
[07949]  {:unknown, ' '}
[07950]  {:unknown, 's'}
[07951]  {:unknown, 'm'}
[07952]  {:unknown, 'a'}
[07953]  {:unknown, 'l'}
[07954]  {:unknown, 'l'}
[07955]  {:unknown, 'e'}
[07956]  {:unknown, 'r'}
[07957]  {:unknown, ','}
[07958]  {:unknown, ' '}
[07959]  {:unknown, 'm'}
[07960]  {:unknown, 'o'}
[07961]  {:unknown, 's'}
[07962]  {:unknown, 's'}
[07963]  {:unknown, '-'}
[07964]  {:unknown, 'f'}
[07965]  {:unknown, 'i'}
[07966]  {:unknown, 'l'}
[07967]  {:unknown, 'l'}
[07968]  {:unknown, 'e'}
[07969]  {:unknown, 'd'}
[07970]  {:unknown, ' '}
[07971]  {:unknown, 'c'}
[07972]  {:unknown, 'a'}
[07973]  {:unknown, 'v'}
[07974]  {:unknown, 'e'}
[07975]  {:unknown, 'r'}
[07976]  {:unknown, 'n'}
[07977]  {:unknown, ' '}
[07978]  {:unknown, 'b'}
[07979]  {:unknown, 'e'}
[07980]  {:unknown, 'l'}
[07981]  {:unknown, 'o'}
[07982]  {:unknown, 'w'}
[07983]  {:unknown, '.'}
[07984]  {:jmp, {:value, 99}}
[07986]  {:unknown, 'a'}
[07987]  {:unknown, 'v'}
[07988]  {:unknown, 'e'}
[07989]  {:unknown, 'r'}
[07990]  {:unknown, 'n'}
[07991]  {:jmp, {:value, 108}}
[07993]  {:unknown, 'a'}
[07994]  {:unknown, 'd'}
[07995]  {:unknown, 'd'}
[07996]  {:unknown, 'e'}
[07997]  {:unknown, 'r'}
[07998]  {:jf, {:value, 100}, {:value, 97}}
[08001]  {:unknown, 'r'}
[08002]  {:unknown, 'k'}
[08003]  {:unknown, 'n'}
[08004]  {:unknown, 'e'}
[08005]  {:unknown, 's'}
[08006]  {:unknown, 's'}
[08007]  {:jt, {:value, 80}, {:value, 97}}
[08010]  {:unknown, 's'}
[08011]  {:unknown, 's'}
[08012]  {:unknown, 'a'}
[08013]  {:unknown, 'g'}
[08014]  {:unknown, 'e'}
[08015]  {:unknown, '9'}
[08016]  {:unknown, 'I'}
[08017]  {:unknown, 't'}
[08018]  {:unknown, ' '}
[08019]  {:unknown, 'i'}
[08020]  {:unknown, 's'}
[08021]  {:unknown, ' '}
[08022]  {:unknown, 'p'}
[08023]  {:unknown, 'i'}
[08024]  {:unknown, 't'}
[08025]  {:unknown, 'c'}
[08026]  {:unknown, 'h'}
[08027]  {:unknown, ' '}
[08028]  {:unknown, 'b'}
[08029]  {:unknown, 'l'}
[08030]  {:unknown, 'a'}
[08031]  {:unknown, 'c'}
[08032]  {:unknown, 'k'}
[08033]  {:unknown, '.'}
[08034]  {:unknown, ' '}
[08035]  {:unknown, ' '}
[08036]  {:unknown, 'Y'}
[08037]  {:unknown, 'o'}
[08038]  {:unknown, 'u'}
[08039]  {:unknown, ' '}
[08040]  {:unknown, 'a'}
[08041]  {:unknown, 'r'}
[08042]  {:unknown, 'e'}
[08043]  {:unknown, ' '}
[08044]  {:unknown, 'l'}
[08045]  {:unknown, 'i'}
[08046]  {:unknown, 'k'}
[08047]  {:unknown, 'e'}
[08048]  {:unknown, 'l'}
[08049]  {:unknown, 'y'}
[08050]  {:unknown, ' '}
[08051]  {:unknown, 't'}
[08052]  {:unknown, 'o'}
[08053]  {:unknown, ' '}
[08054]  {:unknown, 'b'}
[08055]  {:unknown, 'e'}
[08056]  {:unknown, ' '}
[08057]  {:unknown, 'e'}
[08058]  {:unknown, 'a'}
[08059]  {:unknown, 't'}
[08060]  {:unknown, 'e'}
[08061]  {:unknown, 'n'}
[08062]  {:unknown, ' '}
[08063]  {:unknown, 'b'}
[08064]  {:unknown, 'y'}
[08065]  {:unknown, ' '}
[08066]  {:unknown, 'a'}
[08067]  {:unknown, ' '}
[08068]  {:unknown, 'g'}
[08069]  {:unknown, 'r'}
[08070]  {:unknown, 'u'}
[08071]  {:unknown, 'e'}
[08072]  {:unknown, '.'}
[08073]  {:unknown, 'K'}
[08074]  {:unknown, 'Y'}
[08075]  {:unknown, 'o'}
[08076]  {:unknown, 'u'}
[08077]  {:unknown, ' '}
[08078]  {:unknown, 'f'}
[08079]  {:unknown, 'e'}
[08080]  {:unknown, 'e'}
[08081]  {:unknown, 'l'}
[08082]  {:unknown, ' '}
[08083]  {:unknown, 't'}
[08084]  {:unknown, 'h'}
[08085]  {:unknown, 'a'}
[08086]  {:unknown, 't'}
[08087]  {:unknown, ' '}
[08088]  {:unknown, 'y'}
[08089]  {:unknown, 'o'}
[08090]  {:unknown, 'u'}
[08091]  {:unknown, 'r'}
[08092]  {:unknown, ' '}
[08093]  {:unknown, 'l'}
[08094]  {:unknown, 'i'}
[08095]  {:unknown, 'g'}
[08096]  {:unknown, 'h'}
[08097]  {:unknown, 't'}
[08098]  {:unknown, ' '}
[08099]  {:unknown, 's'}
[08100]  {:unknown, 'o'}
[08101]  {:unknown, 'u'}
[08102]  {:unknown, 'r'}
[08103]  {:unknown, 'c'}
[08104]  {:unknown, 'e'}
[08105]  {:unknown, ' '}
[08106]  {:unknown, 'i'}
[08107]  {:unknown, 's'}
[08108]  {:unknown, ' '}
[08109]  {:unknown, 'm'}
[08110]  {:unknown, 'o'}
[08111]  {:unknown, 'r'}
[08112]  {:unknown, 'e'}
[08113]  {:unknown, ' '}
[08114]  {:unknown, 't'}
[08115]  {:unknown, 'h'}
[08116]  {:unknown, 'a'}
[08117]  {:unknown, 'n'}
[08118]  {:unknown, ' '}
[08119]  {:unknown, 's'}
[08120]  {:unknown, 'u'}
[08121]  {:unknown, 'f'}
[08122]  {:unknown, 'f'}
[08123]  {:unknown, 'i'}
[08124]  {:unknown, 'c'}
[08125]  {:unknown, 'i'}
[08126]  {:unknown, 'e'}
[08127]  {:unknown, 'n'}
[08128]  {:unknown, 't'}
[08129]  {:unknown, ' '}
[08130]  {:unknown, 't'}
[08131]  {:unknown, 'o'}
[08132]  {:unknown, ' '}
[08133]  {:unknown, 'k'}
[08134]  {:unknown, 'e'}
[08135]  {:unknown, 'e'}
[08136]  {:unknown, 'p'}
[08137]  {:unknown, ' '}
[08138]  {:unknown, 'g'}
[08139]  {:unknown, 'r'}
[08140]  {:unknown, 'u'}
[08141]  {:unknown, 'e'}
[08142]  {:unknown, 's'}
[08143]  {:unknown, ' '}
[08144]  {:unknown, 'a'}
[08145]  {:unknown, 'w'}
[08146]  {:unknown, 'a'}
[08147]  {:unknown, 'y'}
[08148]  {:unknown, '.'}
[08149]  {:jf, {:value, 99}, {:value, 111}}
[08152]  {:unknown, 'n'}
[08153]  {:unknown, 't'}
[08154]  {:unknown, 'i'}
[08155]  {:unknown, 'n'}
[08156]  {:unknown, 'u'}
[08157]  {:unknown, 'e'}
[08158]  {:eq, {:value, 98}, {:value, 97}, {:value, 99}}
[08162]  {:unknown, 'k'}
[08163]  {:rmem, {:value, 84}, {:value, 119}}
[08166]  {:unknown, 'i'}
[08167]  {:unknown, 's'}
[08168]  {:unknown, 't'}
[08169]  {:unknown, 'y'}
[08170]  {:unknown, ' '}
[08171]  {:unknown, 'p'}
[08172]  {:unknown, 'a'}
[08173]  {:unknown, 's'}
[08174]  {:unknown, 's'}
[08175]  {:unknown, 'a'}
[08176]  {:unknown, 'g'}
[08177]  {:unknown, 'e'}
[08178]  {:unknown, 's'}
[08179]  {:unknown, '{'}
[08180]  {:unknown, 'Y'}
[08181]  {:unknown, 'o'}
[08182]  {:unknown, 'u'}
[08183]  {:unknown, ' '}
[08184]  {:unknown, 'a'}
[08185]  {:unknown, 'r'}
[08186]  {:unknown, 'e'}
[08187]  {:unknown, ' '}
[08188]  {:unknown, 'i'}
[08189]  {:unknown, 'n'}
[08190]  {:unknown, ' '}
[08191]  {:unknown, 'a'}
[08192]  {:unknown, ' '}
[08193]  {:unknown, 'm'}
[08194]  {:unknown, 'a'}
[08195]  {:unknown, 'z'}
[08196]  {:unknown, 'e'}
[08197]  {:unknown, ' '}
[08198]  {:unknown, 'o'}
[08199]  {:unknown, 'f'}
[08200]  {:unknown, ' '}
[08201]  {:unknown, 't'}
[08202]  {:unknown, 'w'}
[08203]  {:unknown, 'i'}
[08204]  {:unknown, 's'}
[08205]  {:unknown, 't'}
[08206]  {:unknown, 'y'}
[08207]  {:unknown, ' '}
[08208]  {:unknown, 'l'}
[08209]  {:unknown, 'i'}
[08210]  {:unknown, 't'}
[08211]  {:unknown, 't'}
[08212]  {:unknown, 'l'}
[08213]  {:unknown, 'e'}
[08214]  {:unknown, ' '}
[08215]  {:unknown, 'p'}
[08216]  {:unknown, 'a'}
[08217]  {:unknown, 's'}
[08218]  {:unknown, 's'}
[08219]  {:unknown, 'a'}
[08220]  {:unknown, 'g'}
[08221]  {:unknown, 'e'}
[08222]  {:unknown, 's'}
[08223]  {:unknown, ','}
[08224]  {:unknown, ' '}
[08225]  {:unknown, 'a'}
[08226]  {:unknown, 'l'}
[08227]  {:unknown, 'l'}
[08228]  {:unknown, ' '}
[08229]  {:unknown, 'd'}
[08230]  {:unknown, 'i'}
[08231]  {:unknown, 'm'}
[08232]  {:unknown, 'l'}
[08233]  {:unknown, 'y'}
[08234]  {:unknown, ' '}
[08235]  {:unknown, 'l'}
[08236]  {:unknown, 'i'}
[08237]  {:unknown, 't'}
[08238]  {:unknown, ' '}
[08239]  {:unknown, 'b'}
[08240]  {:unknown, 'y'}
[08241]  {:unknown, ' '}
[08242]  {:unknown, 'm'}
[08243]  {:unknown, 'o'}
[08244]  {:unknown, 'r'}
[08245]  {:unknown, 'e'}
[08246]  {:unknown, ' '}
[08247]  {:unknown, 'b'}
[08248]  {:unknown, 'i'}
[08249]  {:unknown, 'o'}
[08250]  {:unknown, 'l'}
[08251]  {:unknown, 'u'}
[08252]  {:unknown, 'm'}
[08253]  {:unknown, 'i'}
[08254]  {:unknown, 'n'}
[08255]  {:unknown, 'e'}
[08256]  {:unknown, 's'}
[08257]  {:unknown, 'c'}
[08258]  {:unknown, 'e'}
[08259]  {:unknown, 'n'}
[08260]  {:unknown, 't'}
[08261]  {:unknown, ' '}
[08262]  {:unknown, 'm'}
[08263]  {:unknown, 'o'}
[08264]  {:unknown, 's'}
[08265]  {:unknown, 's'}
[08266]  {:unknown, '.'}
[08267]  {:unknown, ' '}
[08268]  {:unknown, ' '}
[08269]  {:unknown, 'T'}
[08270]  {:unknown, 'h'}
[08271]  {:unknown, 'e'}
[08272]  {:unknown, 'r'}
[08273]  {:unknown, 'e'}
[08274]  {:unknown, ' '}
[08275]  {:unknown, 'i'}
[08276]  {:unknown, 's'}
[08277]  {:unknown, ' '}
[08278]  {:unknown, 'a'}
[08279]  {:unknown, ' '}
[08280]  {:unknown, 'l'}
[08281]  {:unknown, 'a'}
[08282]  {:unknown, 'd'}
[08283]  {:unknown, 'd'}
[08284]  {:unknown, 'e'}
[08285]  {:unknown, 'r'}
[08286]  {:unknown, ' '}
[08287]  {:unknown, 'h'}
[08288]  {:unknown, 'e'}
[08289]  {:unknown, 'r'}
[08290]  {:unknown, 'e'}
[08291]  {:unknown, ' '}
[08292]  {:unknown, 'l'}
[08293]  {:unknown, 'e'}
[08294]  {:unknown, 'a'}
[08295]  {:unknown, 'd'}
[08296]  {:unknown, 'i'}
[08297]  {:unknown, 'n'}
[08298]  {:unknown, 'g'}
[08299]  {:unknown, ' '}
[08300]  {:unknown, 'u'}
[08301]  {:unknown, 'p'}
[08302]  {:unknown, '.'}
[08303]  {:jmp, {:value, 108}}
[08305]  {:unknown, 'a'}
[08306]  {:unknown, 'd'}
[08307]  {:unknown, 'd'}
[08308]  {:unknown, 'e'}
[08309]  {:unknown, 'r'}
[08310]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08314]  {:unknown, 't'}
[08315]  {:unknown, 'h'}
[08316]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08320]  {:unknown, 't'}
[08321]  {:unknown, 'h'}
[08322]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[08326]  {:unknown, 't'}
[08327]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[08331]  {:unknown, 't'}
[08332]  {:rmem, {:value, 84}, {:value, 119}}
[08335]  {:unknown, 'i'}
[08336]  {:unknown, 's'}
[08337]  {:unknown, 't'}
[08338]  {:unknown, 'y'}
[08339]  {:unknown, ' '}
[08340]  {:unknown, 'p'}
[08341]  {:unknown, 'a'}
[08342]  {:unknown, 's'}
[08343]  {:unknown, 's'}
[08344]  {:unknown, 'a'}
[08345]  {:unknown, 'g'}
[08346]  {:unknown, 'e'}
[08347]  {:unknown, 's'}
[08348]  {:unknown, '7'}
[08349]  {:unknown, 'Y'}
[08350]  {:unknown, 'o'}
[08351]  {:unknown, 'u'}
[08352]  {:unknown, ' '}
[08353]  {:unknown, 'a'}
[08354]  {:unknown, 'r'}
[08355]  {:unknown, 'e'}
[08356]  {:unknown, ' '}
[08357]  {:unknown, 'i'}
[08358]  {:unknown, 'n'}
[08359]  {:unknown, ' '}
[08360]  {:unknown, 'a'}
[08361]  {:unknown, ' '}
[08362]  {:unknown, 't'}
[08363]  {:unknown, 'w'}
[08364]  {:unknown, 'i'}
[08365]  {:unknown, 's'}
[08366]  {:unknown, 't'}
[08367]  {:unknown, 'y'}
[08368]  {:unknown, ' '}
[08369]  {:unknown, 'm'}
[08370]  {:unknown, 'a'}
[08371]  {:unknown, 'z'}
[08372]  {:unknown, 'e'}
[08373]  {:unknown, ' '}
[08374]  {:unknown, 'o'}
[08375]  {:unknown, 'f'}
[08376]  {:unknown, ' '}
[08377]  {:unknown, 'l'}
[08378]  {:unknown, 'i'}
[08379]  {:unknown, 't'}
[08380]  {:unknown, 't'}
[08381]  {:unknown, 'l'}
[08382]  {:unknown, 'e'}
[08383]  {:unknown, ' '}
[08384]  {:unknown, 'p'}
[08385]  {:unknown, 'a'}
[08386]  {:unknown, 's'}
[08387]  {:unknown, 's'}
[08388]  {:unknown, 'a'}
[08389]  {:unknown, 'g'}
[08390]  {:unknown, 'e'}
[08391]  {:unknown, 's'}
[08392]  {:unknown, ','}
[08393]  {:unknown, ' '}
[08394]  {:unknown, 'a'}
[08395]  {:unknown, 'l'}
[08396]  {:unknown, 'l'}
[08397]  {:unknown, ' '}
[08398]  {:unknown, 'a'}
[08399]  {:unknown, 'l'}
[08400]  {:unknown, 'i'}
[08401]  {:unknown, 'k'}
[08402]  {:unknown, 'e'}
[08403]  {:unknown, '.'}
[08404]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08408]  {:unknown, 't'}
[08409]  {:unknown, 'h'}
[08410]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08414]  {:unknown, 't'}
[08415]  {:unknown, 'h'}
[08416]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[08420]  {:unknown, 't'}
[08421]  {:rmem, {:value, 84}, {:value, 119}}
[08424]  {:unknown, 'i'}
[08425]  {:unknown, 's'}
[08426]  {:unknown, 't'}
[08427]  {:unknown, 'y'}
[08428]  {:unknown, ' '}
[08429]  {:unknown, 'p'}
[08430]  {:unknown, 'a'}
[08431]  {:unknown, 's'}
[08432]  {:unknown, 's'}
[08433]  {:unknown, 'a'}
[08434]  {:unknown, 'g'}
[08435]  {:unknown, 'e'}
[08436]  {:unknown, 's'}
[08437]  {:unknown, '7'}
[08438]  {:unknown, 'Y'}
[08439]  {:unknown, 'o'}
[08440]  {:unknown, 'u'}
[08441]  {:unknown, ' '}
[08442]  {:unknown, 'a'}
[08443]  {:unknown, 'r'}
[08444]  {:unknown, 'e'}
[08445]  {:unknown, ' '}
[08446]  {:unknown, 'i'}
[08447]  {:unknown, 'n'}
[08448]  {:unknown, ' '}
[08449]  {:unknown, 'a'}
[08450]  {:unknown, ' '}
[08451]  {:unknown, 'm'}
[08452]  {:unknown, 'a'}
[08453]  {:unknown, 'z'}
[08454]  {:unknown, 'e'}
[08455]  {:unknown, ' '}
[08456]  {:unknown, 'o'}
[08457]  {:unknown, 'f'}
[08458]  {:unknown, ' '}
[08459]  {:unknown, 'l'}
[08460]  {:unknown, 'i'}
[08461]  {:unknown, 't'}
[08462]  {:unknown, 't'}
[08463]  {:unknown, 'l'}
[08464]  {:unknown, 'e'}
[08465]  {:unknown, ' '}
[08466]  {:unknown, 't'}
[08467]  {:unknown, 'w'}
[08468]  {:unknown, 'i'}
[08469]  {:unknown, 's'}
[08470]  {:unknown, 't'}
[08471]  {:unknown, 'y'}
[08472]  {:unknown, ' '}
[08473]  {:unknown, 'p'}
[08474]  {:unknown, 'a'}
[08475]  {:unknown, 's'}
[08476]  {:unknown, 's'}
[08477]  {:unknown, 'a'}
[08478]  {:unknown, 'g'}
[08479]  {:unknown, 'e'}
[08480]  {:unknown, 's'}
[08481]  {:unknown, ','}
[08482]  {:unknown, ' '}
[08483]  {:unknown, 'a'}
[08484]  {:unknown, 'l'}
[08485]  {:unknown, 'l'}
[08486]  {:unknown, ' '}
[08487]  {:unknown, 'a'}
[08488]  {:unknown, 'l'}
[08489]  {:unknown, 'i'}
[08490]  {:unknown, 'k'}
[08491]  {:unknown, 'e'}
[08492]  {:unknown, '.'}
[08493]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08497]  {:unknown, 't'}
[08498]  {:unknown, 'h'}
[08499]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08503]  {:unknown, 't'}
[08504]  {:unknown, 'h'}
[08505]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[08509]  {:unknown, 't'}
[08510]  {:rmem, {:value, 84}, {:value, 119}}
[08513]  {:unknown, 'i'}
[08514]  {:unknown, 's'}
[08515]  {:unknown, 't'}
[08516]  {:unknown, 'y'}
[08517]  {:unknown, ' '}
[08518]  {:unknown, 'p'}
[08519]  {:unknown, 'a'}
[08520]  {:unknown, 's'}
[08521]  {:unknown, 's'}
[08522]  {:unknown, 'a'}
[08523]  {:unknown, 'g'}
[08524]  {:unknown, 'e'}
[08525]  {:unknown, 's'}
[08526]  {:unknown, '|'}
[08527]  {:unknown, 'Y'}
[08528]  {:unknown, 'o'}
[08529]  {:unknown, 'u'}
[08530]  {:unknown, ' '}
[08531]  {:unknown, 'a'}
[08532]  {:unknown, 'r'}
[08533]  {:unknown, 'e'}
[08534]  {:unknown, ' '}
[08535]  {:unknown, 'i'}
[08536]  {:unknown, 'n'}
[08537]  {:unknown, ' '}
[08538]  {:unknown, 'a'}
[08539]  {:unknown, ' '}
[08540]  {:unknown, 'm'}
[08541]  {:unknown, 'a'}
[08542]  {:unknown, 'z'}
[08543]  {:unknown, 'e'}
[08544]  {:unknown, ' '}
[08545]  {:unknown, 'o'}
[08546]  {:unknown, 'f'}
[08547]  {:unknown, ' '}
[08548]  {:unknown, 'a'}
[08549]  {:unknown, 'l'}
[08550]  {:unknown, 'i'}
[08551]  {:unknown, 'k'}
[08552]  {:unknown, 'e'}
[08553]  {:unknown, ' '}
[08554]  {:unknown, 'l'}
[08555]  {:unknown, 'i'}
[08556]  {:unknown, 't'}
[08557]  {:unknown, 't'}
[08558]  {:unknown, 'l'}
[08559]  {:unknown, 'e'}
[08560]  {:unknown, ' '}
[08561]  {:unknown, 'p'}
[08562]  {:unknown, 'a'}
[08563]  {:unknown, 's'}
[08564]  {:unknown, 's'}
[08565]  {:unknown, 'a'}
[08566]  {:unknown, 'g'}
[08567]  {:unknown, 'e'}
[08568]  {:unknown, 's'}
[08569]  {:unknown, ','}
[08570]  {:unknown, ' '}
[08571]  {:unknown, 'a'}
[08572]  {:unknown, 'l'}
[08573]  {:unknown, 'l'}
[08574]  {:unknown, ' '}
[08575]  {:unknown, 't'}
[08576]  {:unknown, 'w'}
[08577]  {:unknown, 'i'}
[08578]  {:unknown, 's'}
[08579]  {:unknown, 't'}
[08580]  {:unknown, 'y'}
[08581]  {:unknown, '.'}
[08582]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[08586]  {:unknown, 'e'}
[08587]  {:unknown, ' '}
[08588]  {:unknown, 'p'}
[08589]  {:unknown, 'a'}
[08590]  {:unknown, 's'}
[08591]  {:unknown, 's'}
[08592]  {:unknown, 'a'}
[08593]  {:unknown, 'g'}
[08594]  {:unknown, 'e'}
[08595]  {:unknown, ' '}
[08596]  {:unknown, 't'}
[08597]  {:unknown, 'o'}
[08598]  {:unknown, ' '}
[08599]  {:unknown, 't'}
[08600]  {:unknown, 'h'}
[08601]  {:unknown, 'e'}
[08602]  {:unknown, ' '}
[08603]  {:unknown, 'e'}
[08604]  {:unknown, 'a'}
[08605]  {:unknown, 's'}
[08606]  {:unknown, 't'}
[08607]  {:unknown, ' '}
[08608]  {:unknown, 'l'}
[08609]  {:unknown, 'o'}
[08610]  {:unknown, 'o'}
[08611]  {:unknown, 'k'}
[08612]  {:unknown, 's'}
[08613]  {:unknown, ' '}
[08614]  {:unknown, 'v'}
[08615]  {:unknown, 'e'}
[08616]  {:unknown, 'r'}
[08617]  {:unknown, 'y'}
[08618]  {:unknown, ' '}
[08619]  {:unknown, 'd'}
[08620]  {:unknown, 'a'}
[08621]  {:unknown, 'r'}
[08622]  {:unknown, 'k'}
[08623]  {:unknown, ';'}
[08624]  {:unknown, ' '}
[08625]  {:unknown, 'y'}
[08626]  {:unknown, 'o'}
[08627]  {:unknown, 'u'}
[08628]  {:unknown, ' '}
[08629]  {:unknown, 't'}
[08630]  {:unknown, 'h'}
[08631]  {:unknown, 'i'}
[08632]  {:unknown, 'n'}
[08633]  {:unknown, 'k'}
[08634]  {:unknown, ' '}
[08635]  {:unknown, 'y'}
[08636]  {:unknown, 'o'}
[08637]  {:unknown, 'u'}
[08638]  {:unknown, ' '}
[08639]  {:unknown, 'h'}
[08640]  {:unknown, 'e'}
[08641]  {:unknown, 'a'}
[08642]  {:unknown, 'r'}
[08643]  {:unknown, ' '}
[08644]  {:unknown, 'a'}
[08645]  {:unknown, ' '}
[08646]  {:unknown, 'G'}
[08647]  {:unknown, 'r'}
[08648]  {:unknown, 'u'}
[08649]  {:unknown, 'e'}
[08650]  {:unknown, '.'}
[08651]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08655]  {:unknown, 't'}
[08656]  {:unknown, 'h'}
[08657]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08661]  {:unknown, 't'}
[08662]  {:unknown, 'h'}
[08663]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[08667]  {:unknown, 't'}
[08668]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[08672]  {:unknown, 't'}
[08673]  {:rmem, {:value, 84}, {:value, 119}}
[08676]  {:unknown, 'i'}
[08677]  {:unknown, 's'}
[08678]  {:unknown, 't'}
[08679]  {:unknown, 'y'}
[08680]  {:unknown, ' '}
[08681]  {:unknown, 'p'}
[08682]  {:unknown, 'a'}
[08683]  {:unknown, 's'}
[08684]  {:unknown, 's'}
[08685]  {:unknown, 'a'}
[08686]  {:unknown, 'g'}
[08687]  {:unknown, 'e'}
[08688]  {:unknown, 's'}
[08689]  {:unknown, '7'}
[08690]  {:unknown, 'Y'}
[08691]  {:unknown, 'o'}
[08692]  {:unknown, 'u'}
[08693]  {:unknown, ' '}
[08694]  {:unknown, 'a'}
[08695]  {:unknown, 'r'}
[08696]  {:unknown, 'e'}
[08697]  {:unknown, ' '}
[08698]  {:unknown, 'i'}
[08699]  {:unknown, 'n'}
[08700]  {:unknown, ' '}
[08701]  {:unknown, 'a'}
[08702]  {:unknown, ' '}
[08703]  {:unknown, 'l'}
[08704]  {:unknown, 'i'}
[08705]  {:unknown, 't'}
[08706]  {:unknown, 't'}
[08707]  {:unknown, 'l'}
[08708]  {:unknown, 'e'}
[08709]  {:unknown, ' '}
[08710]  {:unknown, 'm'}
[08711]  {:unknown, 'a'}
[08712]  {:unknown, 'z'}
[08713]  {:unknown, 'e'}
[08714]  {:unknown, ' '}
[08715]  {:unknown, 'o'}
[08716]  {:unknown, 'f'}
[08717]  {:unknown, ' '}
[08718]  {:unknown, 't'}
[08719]  {:unknown, 'w'}
[08720]  {:unknown, 'i'}
[08721]  {:unknown, 's'}
[08722]  {:unknown, 't'}
[08723]  {:unknown, 'y'}
[08724]  {:unknown, ' '}
[08725]  {:unknown, 'p'}
[08726]  {:unknown, 'a'}
[08727]  {:unknown, 's'}
[08728]  {:unknown, 's'}
[08729]  {:unknown, 'a'}
[08730]  {:unknown, 'g'}
[08731]  {:unknown, 'e'}
[08732]  {:unknown, 's'}
[08733]  {:unknown, ','}
[08734]  {:unknown, ' '}
[08735]  {:unknown, 'a'}
[08736]  {:unknown, 'l'}
[08737]  {:unknown, 'l'}
[08738]  {:unknown, ' '}
[08739]  {:unknown, 'a'}
[08740]  {:unknown, 'l'}
[08741]  {:unknown, 'i'}
[08742]  {:unknown, 'k'}
[08743]  {:unknown, 'e'}
[08744]  {:unknown, '.'}
[08745]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08749]  {:unknown, 't'}
[08750]  {:unknown, 'h'}
[08751]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08755]  {:unknown, 't'}
[08756]  {:unknown, 'h'}
[08757]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[08761]  {:unknown, 't'}
[08762]  {:rmem, {:value, 84}, {:value, 119}}
[08765]  {:unknown, 'i'}
[08766]  {:unknown, 's'}
[08767]  {:unknown, 't'}
[08768]  {:unknown, 'y'}
[08769]  {:unknown, ' '}
[08770]  {:unknown, 'p'}
[08771]  {:unknown, 'a'}
[08772]  {:unknown, 's'}
[08773]  {:unknown, 's'}
[08774]  {:unknown, 'a'}
[08775]  {:unknown, 'g'}
[08776]  {:unknown, 'e'}
[08777]  {:unknown, 's'}
[08778]  {:unknown, [131]}
[08779]  {:unknown, 'Y'}
[08780]  {:unknown, 'o'}
[08781]  {:unknown, 'u'}
[08782]  {:unknown, ' '}
[08783]  {:unknown, 'a'}
[08784]  {:unknown, 'r'}
[08785]  {:unknown, 'e'}
[08786]  {:unknown, ' '}
[08787]  {:unknown, 'i'}
[08788]  {:unknown, 'n'}
[08789]  {:unknown, ' '}
[08790]  {:unknown, 'a'}
[08791]  {:unknown, ' '}
[08792]  {:unknown, 't'}
[08793]  {:unknown, 'w'}
[08794]  {:unknown, 'i'}
[08795]  {:unknown, 's'}
[08796]  {:unknown, 't'}
[08797]  {:unknown, 'y'}
[08798]  {:unknown, ' '}
[08799]  {:unknown, 'a'}
[08800]  {:unknown, 'l'}
[08801]  {:unknown, 'i'}
[08802]  {:unknown, 'k'}
[08803]  {:unknown, 'e'}
[08804]  {:unknown, ' '}
[08805]  {:unknown, 'o'}
[08806]  {:unknown, 'f'}
[08807]  {:unknown, ' '}
[08808]  {:unknown, 'l'}
[08809]  {:unknown, 'i'}
[08810]  {:unknown, 't'}
[08811]  {:unknown, 't'}
[08812]  {:unknown, 'l'}
[08813]  {:unknown, 'e'}
[08814]  {:unknown, ' '}
[08815]  {:unknown, 'p'}
[08816]  {:unknown, 'a'}
[08817]  {:unknown, 's'}
[08818]  {:unknown, 's'}
[08819]  {:unknown, 'a'}
[08820]  {:unknown, 'g'}
[08821]  {:unknown, 'e'}
[08822]  {:unknown, 's'}
[08823]  {:unknown, ','}
[08824]  {:unknown, ' '}
[08825]  {:unknown, 'a'}
[08826]  {:unknown, 'l'}
[08827]  {:unknown, 'l'}
[08828]  {:unknown, ' '}
[08829]  {:unknown, 'm'}
[08830]  {:unknown, 'a'}
[08831]  {:unknown, 'z'}
[08832]  {:unknown, 'e'}
[08833]  {:unknown, '.'}
[08834]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[08838]  {:unknown, 'e'}
[08839]  {:unknown, ' '}
[08840]  {:unknown, 'e'}
[08841]  {:unknown, 'a'}
[08842]  {:unknown, 's'}
[08843]  {:unknown, 't'}
[08844]  {:unknown, ' '}
[08845]  {:unknown, 'p'}
[08846]  {:unknown, 'a'}
[08847]  {:unknown, 's'}
[08848]  {:unknown, 's'}
[08849]  {:unknown, 'a'}
[08850]  {:unknown, 'g'}
[08851]  {:unknown, 'e'}
[08852]  {:unknown, ' '}
[08853]  {:unknown, 'a'}
[08854]  {:unknown, 'p'}
[08855]  {:unknown, 'p'}
[08856]  {:unknown, 'e'}
[08857]  {:unknown, 'a'}
[08858]  {:unknown, 'r'}
[08859]  {:unknown, 's'}
[08860]  {:unknown, ' '}
[08861]  {:unknown, 'v'}
[08862]  {:unknown, 'e'}
[08863]  {:unknown, 'r'}
[08864]  {:unknown, 'y'}
[08865]  {:unknown, ' '}
[08866]  {:unknown, 'd'}
[08867]  {:unknown, 'a'}
[08868]  {:unknown, 'r'}
[08869]  {:unknown, 'k'}
[08870]  {:unknown, ';'}
[08871]  {:unknown, ' '}
[08872]  {:unknown, 'y'}
[08873]  {:unknown, 'o'}
[08874]  {:unknown, 'u'}
[08875]  {:unknown, ' '}
[08876]  {:unknown, 'f'}
[08877]  {:unknown, 'e'}
[08878]  {:unknown, 'e'}
[08879]  {:unknown, 'l'}
[08880]  {:unknown, ' '}
[08881]  {:unknown, 'l'}
[08882]  {:unknown, 'i'}
[08883]  {:unknown, 'k'}
[08884]  {:unknown, 'e'}
[08885]  {:unknown, 'l'}
[08886]  {:unknown, 'y'}
[08887]  {:unknown, ' '}
[08888]  {:unknown, 't'}
[08889]  {:unknown, 'o'}
[08890]  {:unknown, ' '}
[08891]  {:unknown, 'b'}
[08892]  {:unknown, 'e'}
[08893]  {:unknown, ' '}
[08894]  {:unknown, 'e'}
[08895]  {:unknown, 'a'}
[08896]  {:unknown, 't'}
[08897]  {:unknown, 'e'}
[08898]  {:unknown, 'n'}
[08899]  {:unknown, ' '}
[08900]  {:unknown, 'b'}
[08901]  {:unknown, 'y'}
[08902]  {:unknown, ' '}
[08903]  {:unknown, 'a'}
[08904]  {:unknown, ' '}
[08905]  {:unknown, 'G'}
[08906]  {:unknown, 'r'}
[08907]  {:unknown, 'u'}
[08908]  {:unknown, 'e'}
[08909]  {:unknown, '.'}
[08910]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[08914]  {:unknown, 't'}
[08915]  {:unknown, 'h'}
[08916]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[08920]  {:unknown, 't'}
[08921]  {:unknown, 'h'}
[08922]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[08926]  {:unknown, 't'}
[08927]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[08931]  {:unknown, 't'}
[08932]  {:rmem, {:value, 84}, {:value, 119}}
[08935]  {:unknown, 'i'}
[08936]  {:unknown, 's'}
[08937]  {:unknown, 't'}
[08938]  {:unknown, 'y'}
[08939]  {:unknown, ' '}
[08940]  {:unknown, 'p'}
[08941]  {:unknown, 'a'}
[08942]  {:unknown, 's'}
[08943]  {:unknown, 's'}
[08944]  {:unknown, 'a'}
[08945]  {:unknown, 'g'}
[08946]  {:unknown, 'e'}
[08947]  {:unknown, 's'}
[08948]  {:unknown, '7'}
[08949]  {:unknown, 'Y'}
[08950]  {:unknown, 'o'}
[08951]  {:unknown, 'u'}
[08952]  {:unknown, ' '}
[08953]  {:unknown, 'a'}
[08954]  {:unknown, 'r'}
[08955]  {:unknown, 'e'}
[08956]  {:unknown, ' '}
[08957]  {:unknown, 'i'}
[08958]  {:unknown, 'n'}
[08959]  {:unknown, ' '}
[08960]  {:unknown, 'a'}
[08961]  {:unknown, ' '}
[08962]  {:unknown, 'm'}
[08963]  {:unknown, 'a'}
[08964]  {:unknown, 'z'}
[08965]  {:unknown, 'e'}
[08966]  {:unknown, ' '}
[08967]  {:unknown, 'o'}
[08968]  {:unknown, 'f'}
[08969]  {:unknown, ' '}
[08970]  {:unknown, 'a'}
[08971]  {:unknown, 'l'}
[08972]  {:unknown, 'i'}
[08973]  {:unknown, 'k'}
[08974]  {:unknown, 'e'}
[08975]  {:unknown, ' '}
[08976]  {:unknown, 't'}
[08977]  {:unknown, 'w'}
[08978]  {:unknown, 'i'}
[08979]  {:unknown, 's'}
[08980]  {:unknown, 't'}
[08981]  {:unknown, 'y'}
[08982]  {:unknown, ' '}
[08983]  {:unknown, 'p'}
[08984]  {:unknown, 'a'}
[08985]  {:unknown, 's'}
[08986]  {:unknown, 's'}
[08987]  {:unknown, 'a'}
[08988]  {:unknown, 'g'}
[08989]  {:unknown, 'e'}
[08990]  {:unknown, 's'}
[08991]  {:unknown, ','}
[08992]  {:unknown, ' '}
[08993]  {:unknown, 'a'}
[08994]  {:unknown, 'l'}
[08995]  {:unknown, 'l'}
[08996]  {:unknown, ' '}
[08997]  {:unknown, 'l'}
[08998]  {:unknown, 'i'}
[08999]  {:unknown, 't'}
[09000]  {:unknown, 't'}
[09001]  {:unknown, 'l'}
[09002]  {:unknown, 'e'}
[09003]  {:unknown, '.'}
[09004]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[09008]  {:unknown, 't'}
[09009]  {:unknown, 'h'}
[09010]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09014]  {:unknown, 't'}
[09015]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[09019]  {:unknown, 't'}
[09020]  {:unknown, 'h'}
[09021]  {:rmem, {:value, 84}, {:value, 119}}
[09024]  {:unknown, 'i'}
[09025]  {:unknown, 's'}
[09026]  {:unknown, 't'}
[09027]  {:unknown, 'y'}
[09028]  {:unknown, ' '}
[09029]  {:unknown, 'p'}
[09030]  {:unknown, 'a'}
[09031]  {:unknown, 's'}
[09032]  {:unknown, 's'}
[09033]  {:unknown, 'a'}
[09034]  {:unknown, 'g'}
[09035]  {:unknown, 'e'}
[09036]  {:unknown, 's'}
[09037]  {:unknown, '7'}
[09038]  {:unknown, 'Y'}
[09039]  {:unknown, 'o'}
[09040]  {:unknown, 'u'}
[09041]  {:unknown, ' '}
[09042]  {:unknown, 'a'}
[09043]  {:unknown, 'r'}
[09044]  {:unknown, 'e'}
[09045]  {:unknown, ' '}
[09046]  {:unknown, 'i'}
[09047]  {:unknown, 'n'}
[09048]  {:unknown, ' '}
[09049]  {:unknown, 'a'}
[09050]  {:unknown, ' '}
[09051]  {:unknown, 'm'}
[09052]  {:unknown, 'a'}
[09053]  {:unknown, 'z'}
[09054]  {:unknown, 'e'}
[09055]  {:unknown, ' '}
[09056]  {:unknown, 'o'}
[09057]  {:unknown, 'f'}
[09058]  {:unknown, ' '}
[09059]  {:unknown, 't'}
[09060]  {:unknown, 'w'}
[09061]  {:unknown, 'i'}
[09062]  {:unknown, 's'}
[09063]  {:unknown, 't'}
[09064]  {:unknown, 'y'}
[09065]  {:unknown, ' '}
[09066]  {:unknown, 'l'}
[09067]  {:unknown, 'i'}
[09068]  {:unknown, 't'}
[09069]  {:unknown, 't'}
[09070]  {:unknown, 'l'}
[09071]  {:unknown, 'e'}
[09072]  {:unknown, ' '}
[09073]  {:unknown, 'p'}
[09074]  {:unknown, 'a'}
[09075]  {:unknown, 's'}
[09076]  {:unknown, 's'}
[09077]  {:unknown, 'a'}
[09078]  {:unknown, 'g'}
[09079]  {:unknown, 'e'}
[09080]  {:unknown, 's'}
[09081]  {:unknown, ','}
[09082]  {:unknown, ' '}
[09083]  {:unknown, 'a'}
[09084]  {:unknown, 'l'}
[09085]  {:unknown, 'l'}
[09086]  {:unknown, ' '}
[09087]  {:unknown, 'a'}
[09088]  {:unknown, 'l'}
[09089]  {:unknown, 'i'}
[09090]  {:unknown, 'k'}
[09091]  {:unknown, 'e'}
[09092]  {:unknown, '.'}
[09093]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09097]  {:unknown, 't'}
[09098]  {:rmem, {:value, 84}, {:value, 119}}
[09101]  {:unknown, 'i'}
[09102]  {:unknown, 's'}
[09103]  {:unknown, 't'}
[09104]  {:unknown, 'y'}
[09105]  {:unknown, ' '}
[09106]  {:unknown, 'p'}
[09107]  {:unknown, 'a'}
[09108]  {:unknown, 's'}
[09109]  {:unknown, 's'}
[09110]  {:unknown, 'a'}
[09111]  {:unknown, 'g'}
[09112]  {:unknown, 'e'}
[09113]  {:unknown, 's'}
[09114]  {:unknown, '7'}
[09115]  {:unknown, 'Y'}
[09116]  {:unknown, 'o'}
[09117]  {:unknown, 'u'}
[09118]  {:unknown, ' '}
[09119]  {:unknown, 'a'}
[09120]  {:unknown, 'r'}
[09121]  {:unknown, 'e'}
[09122]  {:unknown, ' '}
[09123]  {:unknown, 'i'}
[09124]  {:unknown, 'n'}
[09125]  {:unknown, ' '}
[09126]  {:unknown, 'a'}
[09127]  {:unknown, ' '}
[09128]  {:unknown, 'm'}
[09129]  {:unknown, 'a'}
[09130]  {:unknown, 'z'}
[09131]  {:unknown, 'e'}
[09132]  {:unknown, ' '}
[09133]  {:unknown, 'o'}
[09134]  {:unknown, 'f'}
[09135]  {:unknown, ' '}
[09136]  {:unknown, 't'}
[09137]  {:unknown, 'w'}
[09138]  {:unknown, 'i'}
[09139]  {:unknown, 's'}
[09140]  {:unknown, 't'}
[09141]  {:unknown, 'y'}
[09142]  {:unknown, ' '}
[09143]  {:unknown, 'l'}
[09144]  {:unknown, 'i'}
[09145]  {:unknown, 't'}
[09146]  {:unknown, 't'}
[09147]  {:unknown, 'l'}
[09148]  {:unknown, 'e'}
[09149]  {:unknown, ' '}
[09150]  {:unknown, 'p'}
[09151]  {:unknown, 'a'}
[09152]  {:unknown, 's'}
[09153]  {:unknown, 's'}
[09154]  {:unknown, 'a'}
[09155]  {:unknown, 'g'}
[09156]  {:unknown, 'e'}
[09157]  {:unknown, 's'}
[09158]  {:unknown, ','}
[09159]  {:unknown, ' '}
[09160]  {:unknown, 'a'}
[09161]  {:unknown, 'l'}
[09162]  {:unknown, 'l'}
[09163]  {:unknown, ' '}
[09164]  {:unknown, 'a'}
[09165]  {:unknown, 'l'}
[09166]  {:unknown, 'i'}
[09167]  {:unknown, 'k'}
[09168]  {:unknown, 'e'}
[09169]  {:unknown, '.'}
[09170]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09174]  {:unknown, 't'}
[09175]  {:rmem, {:value, 84}, {:value, 119}}
[09178]  {:unknown, 'i'}
[09179]  {:unknown, 's'}
[09180]  {:unknown, 't'}
[09181]  {:unknown, 'y'}
[09182]  {:unknown, ' '}
[09183]  {:unknown, 'p'}
[09184]  {:unknown, 'a'}
[09185]  {:unknown, 's'}
[09186]  {:unknown, 's'}
[09187]  {:unknown, 'a'}
[09188]  {:unknown, 'g'}
[09189]  {:unknown, 'e'}
[09190]  {:unknown, 's'}
[09191]  {:unknown, '7'}
[09192]  {:unknown, 'Y'}
[09193]  {:unknown, 'o'}
[09194]  {:unknown, 'u'}
[09195]  {:unknown, ' '}
[09196]  {:unknown, 'a'}
[09197]  {:unknown, 'r'}
[09198]  {:unknown, 'e'}
[09199]  {:unknown, ' '}
[09200]  {:unknown, 'i'}
[09201]  {:unknown, 'n'}
[09202]  {:unknown, ' '}
[09203]  {:unknown, 'a'}
[09204]  {:unknown, ' '}
[09205]  {:unknown, 't'}
[09206]  {:unknown, 'w'}
[09207]  {:unknown, 'i'}
[09208]  {:unknown, 's'}
[09209]  {:unknown, 't'}
[09210]  {:unknown, 'y'}
[09211]  {:unknown, ' '}
[09212]  {:unknown, 'm'}
[09213]  {:unknown, 'a'}
[09214]  {:unknown, 'z'}
[09215]  {:unknown, 'e'}
[09216]  {:unknown, ' '}
[09217]  {:unknown, 'o'}
[09218]  {:unknown, 'f'}
[09219]  {:unknown, ' '}
[09220]  {:unknown, 'l'}
[09221]  {:unknown, 'i'}
[09222]  {:unknown, 't'}
[09223]  {:unknown, 't'}
[09224]  {:unknown, 'l'}
[09225]  {:unknown, 'e'}
[09226]  {:unknown, ' '}
[09227]  {:unknown, 'p'}
[09228]  {:unknown, 'a'}
[09229]  {:unknown, 's'}
[09230]  {:unknown, 's'}
[09231]  {:unknown, 'a'}
[09232]  {:unknown, 'g'}
[09233]  {:unknown, 'e'}
[09234]  {:unknown, 's'}
[09235]  {:unknown, ','}
[09236]  {:unknown, ' '}
[09237]  {:unknown, 'a'}
[09238]  {:unknown, 'l'}
[09239]  {:unknown, 'l'}
[09240]  {:unknown, ' '}
[09241]  {:unknown, 'a'}
[09242]  {:unknown, 'l'}
[09243]  {:unknown, 'i'}
[09244]  {:unknown, 'k'}
[09245]  {:unknown, 'e'}
[09246]  {:unknown, '.'}
[09247]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[09251]  {:unknown, 't'}
[09252]  {:unknown, 'h'}
[09253]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[09257]  {:unknown, 't'}
[09258]  {:unknown, 'h'}
[09259]  {:and, {:value, 68}, {:value, 97}, {:value, 114}}
[09263]  {:unknown, 'k'}
[09264]  {:unknown, ' '}
[09265]  {:unknown, 'p'}
[09266]  {:unknown, 'a'}
[09267]  {:unknown, 's'}
[09268]  {:unknown, 's'}
[09269]  {:unknown, 'a'}
[09270]  {:unknown, 'g'}
[09271]  {:unknown, 'e'}
[09272]  {:unknown, 'r'}
[09273]  {:unknown, 'Y'}
[09274]  {:unknown, 'o'}
[09275]  {:unknown, 'u'}
[09276]  {:unknown, ' '}
[09277]  {:unknown, 'a'}
[09278]  {:unknown, 'r'}
[09279]  {:unknown, 'e'}
[09280]  {:unknown, ' '}
[09281]  {:unknown, 'i'}
[09282]  {:unknown, 'n'}
[09283]  {:unknown, ' '}
[09284]  {:unknown, 'a'}
[09285]  {:unknown, ' '}
[09286]  {:unknown, 'n'}
[09287]  {:unknown, 'a'}
[09288]  {:unknown, 'r'}
[09289]  {:unknown, 'r'}
[09290]  {:unknown, 'o'}
[09291]  {:unknown, 'w'}
[09292]  {:unknown, ' '}
[09293]  {:unknown, 'p'}
[09294]  {:unknown, 'a'}
[09295]  {:unknown, 's'}
[09296]  {:unknown, 's'}
[09297]  {:unknown, 'a'}
[09298]  {:unknown, 'g'}
[09299]  {:unknown, 'e'}
[09300]  {:unknown, '.'}
[09301]  {:unknown, ' '}
[09302]  {:unknown, ' '}
[09303]  {:unknown, 'T'}
[09304]  {:unknown, 'h'}
[09305]  {:unknown, 'e'}
[09306]  {:unknown, 'r'}
[09307]  {:unknown, 'e'}
[09308]  {:unknown, ' '}
[09309]  {:unknown, 'i'}
[09310]  {:unknown, 's'}
[09311]  {:unknown, ' '}
[09312]  {:unknown, 'd'}
[09313]  {:unknown, 'a'}
[09314]  {:unknown, 'r'}
[09315]  {:unknown, 'k'}
[09316]  {:unknown, 'n'}
[09317]  {:unknown, 'e'}
[09318]  {:unknown, 's'}
[09319]  {:unknown, 's'}
[09320]  {:unknown, ' '}
[09321]  {:unknown, 't'}
[09322]  {:unknown, 'o'}
[09323]  {:unknown, ' '}
[09324]  {:unknown, 't'}
[09325]  {:unknown, 'h'}
[09326]  {:unknown, 'e'}
[09327]  {:unknown, ' '}
[09328]  {:unknown, 'w'}
[09329]  {:unknown, 'e'}
[09330]  {:unknown, 's'}
[09331]  {:unknown, 't'}
[09332]  {:unknown, ','}
[09333]  {:unknown, ' '}
[09334]  {:unknown, 'b'}
[09335]  {:unknown, 'u'}
[09336]  {:unknown, 't'}
[09337]  {:unknown, ' '}
[09338]  {:unknown, 'y'}
[09339]  {:unknown, 'o'}
[09340]  {:unknown, 'u'}
[09341]  {:unknown, ' '}
[09342]  {:unknown, 'c'}
[09343]  {:unknown, 'a'}
[09344]  {:unknown, 'n'}
[09345]  {:unknown, ' '}
[09346]  {:unknown, 'b'}
[09347]  {:unknown, 'a'}
[09348]  {:unknown, 'r'}
[09349]  {:unknown, 'e'}
[09350]  {:unknown, 'l'}
[09351]  {:unknown, 'y'}
[09352]  {:unknown, ' '}
[09353]  {:unknown, 's'}
[09354]  {:unknown, 'e'}
[09355]  {:unknown, 'e'}
[09356]  {:unknown, ' '}
[09357]  {:unknown, 'a'}
[09358]  {:unknown, ' '}
[09359]  {:unknown, 'g'}
[09360]  {:unknown, 'l'}
[09361]  {:unknown, 'o'}
[09362]  {:unknown, 'w'}
[09363]  {:unknown, 'i'}
[09364]  {:unknown, 'n'}
[09365]  {:unknown, 'g'}
[09366]  {:unknown, ' '}
[09367]  {:unknown, 'o'}
[09368]  {:unknown, 'p'}
[09369]  {:unknown, 'e'}
[09370]  {:unknown, 'n'}
[09371]  {:unknown, 'i'}
[09372]  {:unknown, 'n'}
[09373]  {:unknown, 'g'}
[09374]  {:unknown, ' '}
[09375]  {:unknown, 't'}
[09376]  {:unknown, 'o'}
[09377]  {:unknown, ' '}
[09378]  {:unknown, 't'}
[09379]  {:unknown, 'h'}
[09380]  {:unknown, 'e'}
[09381]  {:unknown, ' '}
[09382]  {:unknown, 'e'}
[09383]  {:unknown, 'a'}
[09384]  {:unknown, 's'}
[09385]  {:unknown, 't'}
[09386]  {:unknown, '.'}
[09387]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09391]  {:unknown, 't'}
[09392]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09396]  {:unknown, 't'}
[09397]  {:and, {:value, 68}, {:value, 97}, {:value, 114}}
[09401]  {:unknown, 'k'}
[09402]  {:unknown, ' '}
[09403]  {:unknown, 'p'}
[09404]  {:unknown, 'a'}
[09405]  {:unknown, 's'}
[09406]  {:unknown, 's'}
[09407]  {:unknown, 'a'}
[09408]  {:unknown, 'g'}
[09409]  {:unknown, 'e'}
[09410]  {:unknown, '"'}
[09411]  {:unknown, 'Y'}
[09412]  {:unknown, 'o'}
[09413]  {:unknown, 'u'}
[09414]  {:unknown, ' '}
[09415]  {:unknown, 'a'}
[09416]  {:unknown, 'r'}
[09417]  {:unknown, 'e'}
[09418]  {:unknown, ' '}
[09419]  {:unknown, 'i'}
[09420]  {:unknown, 'n'}
[09421]  {:unknown, ' '}
[09422]  {:unknown, 'a'}
[09423]  {:unknown, ' '}
[09424]  {:unknown, 'd'}
[09425]  {:unknown, 'a'}
[09426]  {:unknown, 'r'}
[09427]  {:unknown, 'k'}
[09428]  {:unknown, ','}
[09429]  {:unknown, ' '}
[09430]  {:unknown, 'n'}
[09431]  {:unknown, 'a'}
[09432]  {:unknown, 'r'}
[09433]  {:unknown, 'r'}
[09434]  {:unknown, 'o'}
[09435]  {:unknown, 'w'}
[09436]  {:unknown, ' '}
[09437]  {:unknown, 'p'}
[09438]  {:unknown, 'a'}
[09439]  {:unknown, 's'}
[09440]  {:unknown, 's'}
[09441]  {:unknown, 'a'}
[09442]  {:unknown, 'g'}
[09443]  {:unknown, 'e'}
[09444]  {:unknown, '.'}
[09445]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09449]  {:unknown, 't'}
[09450]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09454]  {:unknown, 't'}
[09455]  {:and, {:value, 68}, {:value, 97}, {:value, 114}}
[09459]  {:unknown, 'k'}
[09460]  {:unknown, ' '}
[09461]  {:unknown, 'p'}
[09462]  {:unknown, 'a'}
[09463]  {:unknown, 's'}
[09464]  {:unknown, 's'}
[09465]  {:unknown, 'a'}
[09466]  {:unknown, 'g'}
[09467]  {:unknown, 'e'}
[09468]  {:unknown, '"'}
[09469]  {:unknown, 'Y'}
[09470]  {:unknown, 'o'}
[09471]  {:unknown, 'u'}
[09472]  {:unknown, ' '}
[09473]  {:unknown, 'a'}
[09474]  {:unknown, 'r'}
[09475]  {:unknown, 'e'}
[09476]  {:unknown, ' '}
[09477]  {:unknown, 'i'}
[09478]  {:unknown, 'n'}
[09479]  {:unknown, ' '}
[09480]  {:unknown, 'a'}
[09481]  {:unknown, ' '}
[09482]  {:unknown, 'd'}
[09483]  {:unknown, 'a'}
[09484]  {:unknown, 'r'}
[09485]  {:unknown, 'k'}
[09486]  {:unknown, ','}
[09487]  {:unknown, ' '}
[09488]  {:unknown, 'n'}
[09489]  {:unknown, 'a'}
[09490]  {:unknown, 'r'}
[09491]  {:unknown, 'r'}
[09492]  {:unknown, 'o'}
[09493]  {:unknown, 'w'}
[09494]  {:unknown, ' '}
[09495]  {:unknown, 'p'}
[09496]  {:unknown, 'a'}
[09497]  {:unknown, 's'}
[09498]  {:unknown, 's'}
[09499]  {:unknown, 'a'}
[09500]  {:unknown, 'g'}
[09501]  {:unknown, 'e'}
[09502]  {:unknown, '.'}
[09503]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09507]  {:unknown, 't'}
[09508]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09512]  {:unknown, 't'}
[09513]  {:and, {:value, 68}, {:value, 97}, {:value, 114}}
[09517]  {:unknown, 'k'}
[09518]  {:unknown, ' '}
[09519]  {:unknown, 'p'}
[09520]  {:unknown, 'a'}
[09521]  {:unknown, 's'}
[09522]  {:unknown, 's'}
[09523]  {:unknown, 'a'}
[09524]  {:unknown, 'g'}
[09525]  {:unknown, 'e'}
[09526]  {:unknown, 'd'}
[09527]  {:unknown, 'Y'}
[09528]  {:unknown, 'o'}
[09529]  {:unknown, 'u'}
[09530]  {:unknown, ' '}
[09531]  {:unknown, 'a'}
[09532]  {:unknown, 'r'}
[09533]  {:unknown, 'e'}
[09534]  {:unknown, ' '}
[09535]  {:unknown, 'i'}
[09536]  {:unknown, 'n'}
[09537]  {:unknown, ' '}
[09538]  {:unknown, 'a'}
[09539]  {:unknown, ' '}
[09540]  {:unknown, 'd'}
[09541]  {:unknown, 'a'}
[09542]  {:unknown, 'r'}
[09543]  {:unknown, 'k'}
[09544]  {:unknown, ','}
[09545]  {:unknown, ' '}
[09546]  {:unknown, 'n'}
[09547]  {:unknown, 'a'}
[09548]  {:unknown, 'r'}
[09549]  {:unknown, 'r'}
[09550]  {:unknown, 'o'}
[09551]  {:unknown, 'w'}
[09552]  {:unknown, ' '}
[09553]  {:unknown, 'p'}
[09554]  {:unknown, 'a'}
[09555]  {:unknown, 's'}
[09556]  {:unknown, 's'}
[09557]  {:unknown, 'a'}
[09558]  {:unknown, 'g'}
[09559]  {:unknown, 'e'}
[09560]  {:unknown, '.'}
[09561]  {:unknown, ' '}
[09562]  {:unknown, ' '}
[09563]  {:unknown, 'T'}
[09564]  {:unknown, 'o'}
[09565]  {:unknown, ' '}
[09566]  {:unknown, 't'}
[09567]  {:unknown, 'h'}
[09568]  {:unknown, 'e'}
[09569]  {:unknown, ' '}
[09570]  {:unknown, 'w'}
[09571]  {:unknown, 'e'}
[09572]  {:unknown, 's'}
[09573]  {:unknown, 't'}
[09574]  {:unknown, ','}
[09575]  {:unknown, ' '}
[09576]  {:unknown, 'y'}
[09577]  {:unknown, 'o'}
[09578]  {:unknown, 'u'}
[09579]  {:unknown, ' '}
[09580]  {:unknown, 's'}
[09581]  {:unknown, 'p'}
[09582]  {:unknown, 'o'}
[09583]  {:unknown, 't'}
[09584]  {:unknown, ' '}
[09585]  {:unknown, 's'}
[09586]  {:unknown, 'o'}
[09587]  {:unknown, 'm'}
[09588]  {:unknown, 'e'}
[09589]  {:unknown, ' '}
[09590]  {:unknown, 'v'}
[09591]  {:unknown, 'e'}
[09592]  {:unknown, 'g'}
[09593]  {:unknown, 'e'}
[09594]  {:unknown, 't'}
[09595]  {:unknown, 'a'}
[09596]  {:unknown, 't'}
[09597]  {:unknown, 'i'}
[09598]  {:unknown, 'o'}
[09599]  {:unknown, 'n'}
[09600]  {:unknown, ' '}
[09601]  {:unknown, 'w'}
[09602]  {:unknown, 'h'}
[09603]  {:unknown, 'e'}
[09604]  {:unknown, 'r'}
[09605]  {:unknown, 'e'}
[09606]  {:unknown, ' '}
[09607]  {:unknown, 't'}
[09608]  {:unknown, 'h'}
[09609]  {:unknown, 'e'}
[09610]  {:unknown, ' '}
[09611]  {:unknown, 'p'}
[09612]  {:unknown, 'a'}
[09613]  {:unknown, 's'}
[09614]  {:unknown, 's'}
[09615]  {:unknown, 'a'}
[09616]  {:unknown, 'g'}
[09617]  {:unknown, 'e'}
[09618]  {:unknown, ' '}
[09619]  {:unknown, 'e'}
[09620]  {:unknown, 'x'}
[09621]  {:unknown, 'p'}
[09622]  {:unknown, 'a'}
[09623]  {:unknown, 'n'}
[09624]  {:unknown, 'd'}
[09625]  {:unknown, 's'}
[09626]  {:unknown, '.'}
[09627]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09631]  {:unknown, 't'}
[09632]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[09636]  {:unknown, 't'}
[09637]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[09641]  {:unknown, 'n'}
[09642]  {:unknown, 's'}
[09643]  {:unknown, [248]}
[09644]  {:unknown, 'Y'}
[09645]  {:unknown, 'o'}
[09646]  {:unknown, 'u'}
[09647]  {:unknown, ' '}
[09648]  {:unknown, 's'}
[09649]  {:unknown, 't'}
[09650]  {:unknown, 'a'}
[09651]  {:unknown, 'n'}
[09652]  {:unknown, 'd'}
[09653]  {:unknown, ' '}
[09654]  {:unknown, 'i'}
[09655]  {:unknown, 'n'}
[09656]  {:unknown, ' '}
[09657]  {:unknown, 'a'}
[09658]  {:unknown, ' '}
[09659]  {:unknown, 'l'}
[09660]  {:unknown, 'a'}
[09661]  {:unknown, 'r'}
[09662]  {:unknown, 'g'}
[09663]  {:unknown, 'e'}
[09664]  {:unknown, ' '}
[09665]  {:unknown, 'c'}
[09666]  {:unknown, 'a'}
[09667]  {:unknown, 'v'}
[09668]  {:unknown, 'e'}
[09669]  {:unknown, 'r'}
[09670]  {:unknown, 'n'}
[09671]  {:unknown, ' '}
[09672]  {:unknown, 'w'}
[09673]  {:unknown, 'i'}
[09674]  {:unknown, 't'}
[09675]  {:unknown, 'h'}
[09676]  {:unknown, ' '}
[09677]  {:unknown, 'a'}
[09678]  {:unknown, ' '}
[09679]  {:unknown, 'h'}
[09680]  {:unknown, 'u'}
[09681]  {:unknown, 'g'}
[09682]  {:unknown, 'e'}
[09683]  {:unknown, ' '}
[09684]  {:unknown, 'r'}
[09685]  {:unknown, 'u'}
[09686]  {:unknown, 'i'}
[09687]  {:unknown, 'n'}
[09688]  {:unknown, ' '}
[09689]  {:unknown, 't'}
[09690]  {:unknown, 'o'}
[09691]  {:unknown, ' '}
[09692]  {:unknown, 't'}
[09693]  {:unknown, 'h'}
[09694]  {:unknown, 'e'}
[09695]  {:unknown, ' '}
[09696]  {:unknown, 'n'}
[09697]  {:unknown, 'o'}
[09698]  {:unknown, 'r'}
[09699]  {:unknown, 't'}
[09700]  {:unknown, 'h'}
[09701]  {:unknown, ','}
[09702]  {:unknown, ' '}
[09703]  {:unknown, 'o'}
[09704]  {:unknown, 'v'}
[09705]  {:unknown, 'e'}
[09706]  {:unknown, 'r'}
[09707]  {:unknown, 'g'}
[09708]  {:unknown, 'r'}
[09709]  {:unknown, 'o'}
[09710]  {:unknown, 'w'}
[09711]  {:unknown, 'n'}
[09712]  {:unknown, ' '}
[09713]  {:unknown, 'b'}
[09714]  {:unknown, 'y'}
[09715]  {:unknown, ' '}
[09716]  {:unknown, 'p'}
[09717]  {:unknown, 'l'}
[09718]  {:unknown, 'a'}
[09719]  {:unknown, 'n'}
[09720]  {:unknown, 't'}
[09721]  {:unknown, ' '}
[09722]  {:unknown, 'l'}
[09723]  {:unknown, 'i'}
[09724]  {:unknown, 'f'}
[09725]  {:unknown, 'e'}
[09726]  {:unknown, '.'}
[09727]  {:unknown, ' '}
[09728]  {:unknown, ' '}
[09729]  {:unknown, 'T'}
[09730]  {:unknown, 'h'}
[09731]  {:unknown, 'e'}
[09732]  {:unknown, 'r'}
[09733]  {:unknown, 'e'}
[09734]  {:unknown, ' '}
[09735]  {:unknown, 'i'}
[09736]  {:unknown, 's'}
[09737]  {:unknown, ' '}
[09738]  {:unknown, 'a'}
[09739]  {:unknown, ' '}
[09740]  {:unknown, 'l'}
[09741]  {:unknown, 'a'}
[09742]  {:unknown, 'r'}
[09743]  {:unknown, 'g'}
[09744]  {:unknown, 'e'}
[09745]  {:unknown, ' '}
[09746]  {:unknown, 's'}
[09747]  {:unknown, 't'}
[09748]  {:unknown, 'o'}
[09749]  {:unknown, 'n'}
[09750]  {:unknown, 'e'}
[09751]  {:unknown, ' '}
[09752]  {:unknown, 'a'}
[09753]  {:unknown, 'r'}
[09754]  {:unknown, 'c'}
[09755]  {:unknown, 'h'}
[09756]  {:unknown, 'w'}
[09757]  {:unknown, 'a'}
[09758]  {:unknown, 'y'}
[09759]  {:unknown, ' '}
[09760]  {:unknown, 't'}
[09761]  {:unknown, 'o'}
[09762]  {:unknown, ' '}
[09763]  {:unknown, 't'}
[09764]  {:unknown, 'h'}
[09765]  {:unknown, 'e'}
[09766]  {:unknown, ' '}
[09767]  {:unknown, 'n'}
[09768]  {:unknown, 'o'}
[09769]  {:unknown, 'r'}
[09770]  {:unknown, 't'}
[09771]  {:unknown, 'h'}
[09772]  {:unknown, ' '}
[09773]  {:unknown, 'a'}
[09774]  {:unknown, 'c'}
[09775]  {:unknown, 't'}
[09776]  {:unknown, 'i'}
[09777]  {:unknown, 'n'}
[09778]  {:unknown, 'g'}
[09779]  {:unknown, ' '}
[09780]  {:unknown, 'a'}
[09781]  {:unknown, 's'}
[09782]  {:unknown, ' '}
[09783]  {:unknown, 't'}
[09784]  {:unknown, 'h'}
[09785]  {:unknown, 'e'}
[09786]  {:unknown, ' '}
[09787]  {:unknown, 'd'}
[09788]  {:unknown, 'o'}
[09789]  {:unknown, 'o'}
[09790]  {:unknown, 'r'}
[09791]  {:unknown, 'w'}
[09792]  {:unknown, 'a'}
[09793]  {:unknown, 'y'}
[09794]  {:unknown, ' '}
[09795]  {:unknown, 't'}
[09796]  {:unknown, 'o'}
[09797]  {:unknown, ' '}
[09798]  {:unknown, 't'}
[09799]  {:unknown, 'h'}
[09800]  {:unknown, 'e'}
[09801]  {:unknown, ' '}
[09802]  {:unknown, 'r'}
[09803]  {:unknown, 'u'}
[09804]  {:unknown, 'i'}
[09805]  {:unknown, 'n'}
[09806]  {:unknown, 'e'}
[09807]  {:unknown, 'd'}
[09808]  {:unknown, ' '}
[09809]  {:unknown, 'c'}
[09810]  {:unknown, 'o'}
[09811]  {:unknown, 'm'}
[09812]  {:unknown, 'p'}
[09813]  {:unknown, 'l'}
[09814]  {:unknown, 'e'}
[09815]  {:unknown, 'x'}
[09816]  {:unknown, '.'}
[09817]  {:unknown, ' '}
[09818]  {:unknown, ' '}
[09819]  {:unknown, 'A'}
[09820]  {:unknown, ' '}
[09821]  {:unknown, 'c'}
[09822]  {:unknown, 'r'}
[09823]  {:unknown, 'e'}
[09824]  {:unknown, 'v'}
[09825]  {:unknown, 'i'}
[09826]  {:unknown, 'c'}
[09827]  {:unknown, 'e'}
[09828]  {:unknown, ' '}
[09829]  {:unknown, 'i'}
[09830]  {:unknown, 'n'}
[09831]  {:unknown, ' '}
[09832]  {:unknown, 't'}
[09833]  {:unknown, 'h'}
[09834]  {:unknown, 'e'}
[09835]  {:unknown, ' '}
[09836]  {:unknown, 'r'}
[09837]  {:unknown, 'o'}
[09838]  {:unknown, 'c'}
[09839]  {:unknown, 'k'}
[09840]  {:unknown, ' '}
[09841]  {:unknown, 't'}
[09842]  {:unknown, 'o'}
[09843]  {:unknown, ' '}
[09844]  {:unknown, 't'}
[09845]  {:unknown, 'h'}
[09846]  {:unknown, 'e'}
[09847]  {:unknown, ' '}
[09848]  {:unknown, 'e'}
[09849]  {:unknown, 'a'}
[09850]  {:unknown, 's'}
[09851]  {:unknown, 't'}
[09852]  {:unknown, ' '}
[09853]  {:unknown, 'l'}
[09854]  {:unknown, 'e'}
[09855]  {:unknown, 'a'}
[09856]  {:unknown, 'd'}
[09857]  {:unknown, 's'}
[09858]  {:unknown, ' '}
[09859]  {:unknown, 't'}
[09860]  {:unknown, 'o'}
[09861]  {:unknown, ' '}
[09862]  {:unknown, 'a'}
[09863]  {:unknown, 'n'}
[09864]  {:unknown, ' '}
[09865]  {:unknown, 'a'}
[09866]  {:unknown, 'l'}
[09867]  {:unknown, 'a'}
[09868]  {:unknown, 'r'}
[09869]  {:unknown, 'm'}
[09870]  {:unknown, 'i'}
[09871]  {:unknown, 'n'}
[09872]  {:unknown, 'g'}
[09873]  {:unknown, 'l'}
[09874]  {:unknown, 'y'}
[09875]  {:unknown, ' '}
[09876]  {:unknown, 'd'}
[09877]  {:unknown, 'a'}
[09878]  {:unknown, 'r'}
[09879]  {:unknown, 'k'}
[09880]  {:unknown, ' '}
[09881]  {:unknown, 'p'}
[09882]  {:unknown, 'a'}
[09883]  {:unknown, 's'}
[09884]  {:unknown, 's'}
[09885]  {:unknown, 'a'}
[09886]  {:unknown, 'g'}
[09887]  {:unknown, 'e'}
[09888]  {:unknown, 'w'}
[09889]  {:unknown, 'a'}
[09890]  {:unknown, 'y'}
[09891]  {:unknown, '.'}
[09892]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[09896]  {:unknown, 't'}
[09897]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[09901]  {:unknown, 't'}
[09902]  {:unknown, 'h'}
[09903]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[09907]  {:unknown, 'n'}
[09908]  {:unknown, 's'}
[09909]  {:unknown, [181]}
[09910]  {:unknown, 'Y'}
[09911]  {:unknown, 'o'}
[09912]  {:unknown, 'u'}
[09913]  {:unknown, ' '}
[09914]  {:unknown, 'a'}
[09915]  {:unknown, 'r'}
[09916]  {:unknown, 'e'}
[09917]  {:unknown, ' '}
[09918]  {:unknown, 'i'}
[09919]  {:unknown, 'n'}
[09920]  {:unknown, ' '}
[09921]  {:unknown, 't'}
[09922]  {:unknown, 'h'}
[09923]  {:unknown, 'e'}
[09924]  {:unknown, ' '}
[09925]  {:unknown, 'o'}
[09926]  {:unknown, 'n'}
[09927]  {:unknown, 'c'}
[09928]  {:unknown, 'e'}
[09929]  {:unknown, '-'}
[09930]  {:unknown, 'o'}
[09931]  {:unknown, 'p'}
[09932]  {:unknown, 'u'}
[09933]  {:unknown, 'l'}
[09934]  {:unknown, 'e'}
[09935]  {:unknown, 'n'}
[09936]  {:unknown, 't'}
[09937]  {:unknown, ' '}
[09938]  {:unknown, 'f'}
[09939]  {:unknown, 'o'}
[09940]  {:unknown, 'y'}
[09941]  {:unknown, 'e'}
[09942]  {:unknown, 'r'}
[09943]  {:unknown, ' '}
[09944]  {:unknown, 'o'}
[09945]  {:unknown, 'f'}
[09946]  {:unknown, ' '}
[09947]  {:unknown, 'a'}
[09948]  {:unknown, ' '}
[09949]  {:unknown, 'm'}
[09950]  {:unknown, 'a'}
[09951]  {:unknown, 's'}
[09952]  {:unknown, 's'}
[09953]  {:unknown, 'i'}
[09954]  {:unknown, 'v'}
[09955]  {:unknown, 'e'}
[09956]  {:unknown, ' '}
[09957]  {:unknown, 'r'}
[09958]  {:unknown, 'u'}
[09959]  {:unknown, 'i'}
[09960]  {:unknown, 'n'}
[09961]  {:unknown, 'e'}
[09962]  {:unknown, 'd'}
[09963]  {:unknown, ' '}
[09964]  {:unknown, 'c'}
[09965]  {:unknown, 'o'}
[09966]  {:unknown, 'm'}
[09967]  {:unknown, 'p'}
[09968]  {:unknown, 'l'}
[09969]  {:unknown, 'e'}
[09970]  {:unknown, 'x'}
[09971]  {:unknown, '.'}
[09972]  {:unknown, ' '}
[09973]  {:unknown, ' '}
[09974]  {:unknown, 'T'}
[09975]  {:unknown, 'h'}
[09976]  {:unknown, 'e'}
[09977]  {:unknown, 'r'}
[09978]  {:unknown, 'e'}
[09979]  {:unknown, ' '}
[09980]  {:unknown, 'i'}
[09981]  {:unknown, 's'}
[09982]  {:unknown, ' '}
[09983]  {:unknown, 'a'}
[09984]  {:unknown, ' '}
[09985]  {:unknown, 'd'}
[09986]  {:unknown, 'o'}
[09987]  {:unknown, 'o'}
[09988]  {:unknown, 'r'}
[09989]  {:unknown, ' '}
[09990]  {:unknown, 't'}
[09991]  {:unknown, 'o'}
[09992]  {:unknown, ' '}
[09993]  {:unknown, 't'}
[09994]  {:unknown, 'h'}
[09995]  {:unknown, 'e'}
[09996]  {:unknown, ' '}
[09997]  {:unknown, 's'}
[09998]  {:unknown, 'o'}
[09999]  {:unknown, 'u'}
[10000]  {:unknown, 't'}
[10001]  {:unknown, 'h'}
[10002]  {:unknown, ' '}
[10003]  {:unknown, 'l'}
[10004]  {:unknown, 'e'}
[10005]  {:unknown, 'a'}
[10006]  {:unknown, 'd'}
[10007]  {:unknown, 'i'}
[10008]  {:unknown, 'n'}
[10009]  {:unknown, 'g'}
[10010]  {:unknown, ' '}
[10011]  {:unknown, 't'}
[10012]  {:unknown, 'o'}
[10013]  {:unknown, ' '}
[10014]  {:unknown, 't'}
[10015]  {:unknown, 'h'}
[10016]  {:unknown, 'e'}
[10017]  {:unknown, ' '}
[10018]  {:unknown, 'o'}
[10019]  {:unknown, 'v'}
[10020]  {:unknown, 'e'}
[10021]  {:unknown, 'r'}
[10022]  {:unknown, 'g'}
[10023]  {:unknown, 'r'}
[10024]  {:unknown, 'o'}
[10025]  {:unknown, 'w'}
[10026]  {:unknown, 't'}
[10027]  {:unknown, 'h'}
[10028]  {:unknown, ' '}
[10029]  {:unknown, 'o'}
[10030]  {:unknown, 'u'}
[10031]  {:unknown, 't'}
[10032]  {:unknown, 's'}
[10033]  {:unknown, 'i'}
[10034]  {:unknown, 'd'}
[10035]  {:unknown, 'e'}
[10036]  {:unknown, ' '}
[10037]  {:unknown, 'a'}
[10038]  {:unknown, 'n'}
[10039]  {:unknown, 'd'}
[10040]  {:unknown, ' '}
[10041]  {:unknown, 's'}
[10042]  {:unknown, 't'}
[10043]  {:unknown, 'a'}
[10044]  {:unknown, 'i'}
[10045]  {:unknown, 'r'}
[10046]  {:unknown, 's'}
[10047]  {:unknown, ' '}
[10048]  {:unknown, 't'}
[10049]  {:unknown, 'o'}
[10050]  {:unknown, ' '}
[10051]  {:unknown, 't'}
[10052]  {:unknown, 'h'}
[10053]  {:unknown, 'e'}
[10054]  {:unknown, ' '}
[10055]  {:unknown, 'n'}
[10056]  {:unknown, 'o'}
[10057]  {:unknown, 'r'}
[10058]  {:unknown, 't'}
[10059]  {:unknown, 'h'}
[10060]  {:unknown, ' '}
[10061]  {:unknown, 'w'}
[10062]  {:unknown, 'h'}
[10063]  {:unknown, 'i'}
[10064]  {:unknown, 'c'}
[10065]  {:unknown, 'h'}
[10066]  {:unknown, ' '}
[10067]  {:unknown, 'l'}
[10068]  {:unknown, 'e'}
[10069]  {:unknown, 'a'}
[10070]  {:unknown, 'd'}
[10071]  {:unknown, ' '}
[10072]  {:unknown, 'i'}
[10073]  {:unknown, 'n'}
[10074]  {:unknown, 't'}
[10075]  {:unknown, 'o'}
[10076]  {:unknown, ' '}
[10077]  {:unknown, 'a'}
[10078]  {:unknown, ' '}
[10079]  {:unknown, 'l'}
[10080]  {:unknown, 'a'}
[10081]  {:unknown, 'r'}
[10082]  {:unknown, 'g'}
[10083]  {:unknown, 'e'}
[10084]  {:unknown, 'r'}
[10085]  {:unknown, ' '}
[10086]  {:unknown, 'h'}
[10087]  {:unknown, 'a'}
[10088]  {:unknown, 'l'}
[10089]  {:unknown, 'l'}
[10090]  {:unknown, '.'}
[10091]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[10095]  {:unknown, 't'}
[10096]  {:unknown, 'h'}
[10097]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[10101]  {:unknown, 't'}
[10102]  {:unknown, 'h'}
[10103]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[10107]  {:unknown, 'n'}
[10108]  {:unknown, 's'}
[10109]  {:unknown, [293]}
[10110]  {:unknown, 'Y'}
[10111]  {:unknown, 'o'}
[10112]  {:unknown, 'u'}
[10113]  {:unknown, ' '}
[10114]  {:unknown, 's'}
[10115]  {:unknown, 't'}
[10116]  {:unknown, 'a'}
[10117]  {:unknown, 'n'}
[10118]  {:unknown, 'd'}
[10119]  {:unknown, ' '}
[10120]  {:unknown, 'i'}
[10121]  {:unknown, 'n'}
[10122]  {:unknown, ' '}
[10123]  {:unknown, 't'}
[10124]  {:unknown, 'h'}
[10125]  {:unknown, 'e'}
[10126]  {:unknown, ' '}
[10127]  {:unknown, 'm'}
[10128]  {:unknown, 'a'}
[10129]  {:unknown, 's'}
[10130]  {:unknown, 's'}
[10131]  {:unknown, 'i'}
[10132]  {:unknown, 'v'}
[10133]  {:unknown, 'e'}
[10134]  {:unknown, ' '}
[10135]  {:unknown, 'c'}
[10136]  {:unknown, 'e'}
[10137]  {:unknown, 'n'}
[10138]  {:unknown, 't'}
[10139]  {:unknown, 'r'}
[10140]  {:unknown, 'a'}
[10141]  {:unknown, 'l'}
[10142]  {:unknown, ' '}
[10143]  {:unknown, 'h'}
[10144]  {:unknown, 'a'}
[10145]  {:unknown, 'l'}
[10146]  {:unknown, 'l'}
[10147]  {:unknown, ' '}
[10148]  {:unknown, 'o'}
[10149]  {:unknown, 'f'}
[10150]  {:unknown, ' '}
[10151]  {:unknown, 't'}
[10152]  {:unknown, 'h'}
[10153]  {:unknown, 'e'}
[10154]  {:unknown, 's'}
[10155]  {:unknown, 'e'}
[10156]  {:unknown, ' '}
[10157]  {:unknown, 'r'}
[10158]  {:unknown, 'u'}
[10159]  {:unknown, 'i'}
[10160]  {:unknown, 'n'}
[10161]  {:unknown, 's'}
[10162]  {:unknown, '.'}
[10163]  {:unknown, ' '}
[10164]  {:unknown, ' '}
[10165]  {:unknown, 'T'}
[10166]  {:unknown, 'h'}
[10167]  {:unknown, 'e'}
[10168]  {:unknown, ' '}
[10169]  {:unknown, 'w'}
[10170]  {:unknown, 'a'}
[10171]  {:unknown, 'l'}
[10172]  {:unknown, 'l'}
[10173]  {:unknown, 's'}
[10174]  {:unknown, ' '}
[10175]  {:unknown, 'a'}
[10176]  {:unknown, 'r'}
[10177]  {:unknown, 'e'}
[10178]  {:unknown, ' '}
[10179]  {:unknown, 'c'}
[10180]  {:unknown, 'r'}
[10181]  {:unknown, 'u'}
[10182]  {:unknown, 'm'}
[10183]  {:unknown, 'b'}
[10184]  {:unknown, 'l'}
[10185]  {:unknown, 'i'}
[10186]  {:unknown, 'n'}
[10187]  {:unknown, 'g'}
[10188]  {:unknown, ','}
[10189]  {:unknown, ' '}
[10190]  {:unknown, 'a'}
[10191]  {:unknown, 'n'}
[10192]  {:unknown, 'd'}
[10193]  {:unknown, ' '}
[10194]  {:unknown, 'v'}
[10195]  {:unknown, 'e'}
[10196]  {:unknown, 'g'}
[10197]  {:unknown, 'e'}
[10198]  {:unknown, 't'}
[10199]  {:unknown, 'a'}
[10200]  {:unknown, 't'}
[10201]  {:unknown, 'i'}
[10202]  {:unknown, 'o'}
[10203]  {:unknown, 'n'}
[10204]  {:unknown, ' '}
[10205]  {:unknown, 'h'}
[10206]  {:unknown, 'a'}
[10207]  {:unknown, 's'}
[10208]  {:unknown, ' '}
[10209]  {:unknown, 'c'}
[10210]  {:unknown, 'l'}
[10211]  {:unknown, 'e'}
[10212]  {:unknown, 'a'}
[10213]  {:unknown, 'r'}
[10214]  {:unknown, 'l'}
[10215]  {:unknown, 'y'}
[10216]  {:unknown, ' '}
[10217]  {:unknown, 't'}
[10218]  {:unknown, 'a'}
[10219]  {:unknown, 'k'}
[10220]  {:unknown, 'e'}
[10221]  {:unknown, 'n'}
[10222]  {:unknown, ' '}
[10223]  {:unknown, 'o'}
[10224]  {:unknown, 'v'}
[10225]  {:unknown, 'e'}
[10226]  {:unknown, 'r'}
[10227]  {:unknown, '.'}
[10228]  {:unknown, ' '}
[10229]  {:unknown, ' '}
[10230]  {:unknown, 'R'}
[10231]  {:unknown, 'o'}
[10232]  {:unknown, 'o'}
[10233]  {:unknown, 'm'}
[10234]  {:unknown, 's'}
[10235]  {:unknown, ' '}
[10236]  {:unknown, 'a'}
[10237]  {:unknown, 'r'}
[10238]  {:unknown, 'e'}
[10239]  {:unknown, ' '}
[10240]  {:unknown, 'a'}
[10241]  {:unknown, 't'}
[10242]  {:unknown, 't'}
[10243]  {:unknown, 'a'}
[10244]  {:unknown, 'c'}
[10245]  {:unknown, 'h'}
[10246]  {:unknown, 'e'}
[10247]  {:unknown, 'd'}
[10248]  {:unknown, ' '}
[10249]  {:unknown, 'i'}
[10250]  {:unknown, 'n'}
[10251]  {:unknown, ' '}
[10252]  {:unknown, 'a'}
[10253]  {:unknown, 'l'}
[10254]  {:unknown, 'l'}
[10255]  {:unknown, ' '}
[10256]  {:unknown, 'd'}
[10257]  {:unknown, 'i'}
[10258]  {:unknown, 'r'}
[10259]  {:unknown, 'e'}
[10260]  {:unknown, 'c'}
[10261]  {:unknown, 't'}
[10262]  {:unknown, 'i'}
[10263]  {:unknown, 'o'}
[10264]  {:unknown, 'n'}
[10265]  {:unknown, 's'}
[10266]  {:unknown, '.'}
[10267]  {:unknown, ' '}
[10268]  {:unknown, ' '}
[10269]  {:unknown, 'T'}
[10270]  {:unknown, 'h'}
[10271]  {:unknown, 'e'}
[10272]  {:unknown, 'r'}
[10273]  {:unknown, 'e'}
[10274]  {:unknown, ' '}
[10275]  {:unknown, 'i'}
[10276]  {:unknown, 's'}
[10277]  {:unknown, ' '}
[10278]  {:unknown, 'a'}
[10279]  {:unknown, ' '}
[10280]  {:unknown, 's'}
[10281]  {:unknown, 't'}
[10282]  {:unknown, 'r'}
[10283]  {:unknown, 'a'}
[10284]  {:unknown, 'n'}
[10285]  {:unknown, 'g'}
[10286]  {:unknown, 'e'}
[10287]  {:unknown, ' '}
[10288]  {:unknown, 'm'}
[10289]  {:unknown, 'o'}
[10290]  {:unknown, 'n'}
[10291]  {:unknown, 'u'}
[10292]  {:unknown, 'm'}
[10293]  {:unknown, 'e'}
[10294]  {:unknown, 'n'}
[10295]  {:unknown, 't'}
[10296]  {:unknown, ' '}
[10297]  {:unknown, 'i'}
[10298]  {:unknown, 'n'}
[10299]  {:unknown, ' '}
[10300]  {:unknown, 't'}
[10301]  {:unknown, 'h'}
[10302]  {:unknown, 'e'}
[10303]  {:unknown, ' '}
[10304]  {:unknown, 'c'}
[10305]  {:unknown, 'e'}
[10306]  {:unknown, 'n'}
[10307]  {:unknown, 't'}
[10308]  {:unknown, 'e'}
[10309]  {:unknown, 'r'}
[10310]  {:unknown, ' '}
[10311]  {:unknown, 'o'}
[10312]  {:unknown, 'f'}
[10313]  {:unknown, ' '}
[10314]  {:unknown, 't'}
[10315]  {:unknown, 'h'}
[10316]  {:unknown, 'e'}
[10317]  {:unknown, ' '}
[10318]  {:unknown, 'h'}
[10319]  {:unknown, 'a'}
[10320]  {:unknown, 'l'}
[10321]  {:unknown, 'l'}
[10322]  {:unknown, ' '}
[10323]  {:unknown, 'w'}
[10324]  {:unknown, 'i'}
[10325]  {:unknown, 't'}
[10326]  {:unknown, 'h'}
[10327]  {:unknown, ' '}
[10328]  {:unknown, 'c'}
[10329]  {:unknown, 'i'}
[10330]  {:unknown, 'r'}
[10331]  {:unknown, 'c'}
[10332]  {:unknown, 'u'}
[10333]  {:unknown, 'l'}
[10334]  {:unknown, 'a'}
[10335]  {:unknown, 'r'}
[10336]  {:unknown, ' '}
[10337]  {:unknown, 's'}
[10338]  {:unknown, 'l'}
[10339]  {:unknown, 'o'}
[10340]  {:unknown, 't'}
[10341]  {:unknown, 's'}
[10342]  {:unknown, ' '}
[10343]  {:unknown, 'a'}
[10344]  {:unknown, 'n'}
[10345]  {:unknown, 'd'}
[10346]  {:unknown, ' '}
[10347]  {:unknown, 'u'}
[10348]  {:unknown, 'n'}
[10349]  {:unknown, 'u'}
[10350]  {:unknown, 's'}
[10351]  {:unknown, 'u'}
[10352]  {:unknown, 'a'}
[10353]  {:unknown, 'l'}
[10354]  {:unknown, ' '}
[10355]  {:unknown, 's'}
[10356]  {:unknown, 'y'}
[10357]  {:unknown, 'm'}
[10358]  {:unknown, 'b'}
[10359]  {:unknown, 'o'}
[10360]  {:unknown, 'l'}
[10361]  {:unknown, 's'}
[10362]  {:unknown, '.'}
[10363]  {:unknown, ' '}
[10364]  {:unknown, ' '}
[10365]  {:unknown, 'I'}
[10366]  {:unknown, 't'}
[10367]  {:unknown, ' '}
[10368]  {:unknown, 'r'}
[10369]  {:unknown, 'e'}
[10370]  {:unknown, 'a'}
[10371]  {:unknown, 'd'}
[10372]  {:unknown, 's'}
[10373]  {:unknown, ':'}
[10374]  {:mult, {:value, 10}, {:value, 95}, {:value, 32}}
[10378]  {:unknown, '+'}
[10379]  {:unknown, ' '}
[10380]  {:unknown, '_'}
[10381]  {:unknown, ' '}
[10382]  {:unknown, '*'}
[10383]  {:unknown, ' '}
[10384]  {:unknown, '_'}
[10385]  {:unknown, '^'}
[10386]  {:unknown, '2'}
[10387]  {:unknown, ' '}
[10388]  {:unknown, '+'}
[10389]  {:unknown, ' '}
[10390]  {:unknown, '_'}
[10391]  {:unknown, '^'}
[10392]  {:unknown, '3'}
[10393]  {:unknown, ' '}
[10394]  {:unknown, '-'}
[10395]  {:unknown, ' '}
[10396]  {:unknown, '_'}
[10397]  {:unknown, ' '}
[10398]  {:unknown, '='}
[10399]  {:unknown, ' '}
[10400]  {:unknown, '3'}
[10401]  {:unknown, '9'}
[10402]  {:unknown, '9'}
[10403]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[10407]  {:unknown, 't'}
[10408]  {:unknown, 'h'}
[10409]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[10413]  {:unknown, 't'}
[10414]  {:unknown, 'h'}
[10415]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[10419]  {:unknown, 't'}
[10420]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[10424]  {:unknown, 't'}
[10425]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[10429]  {:unknown, 'n'}
[10430]  {:unknown, 's'}
[10431]  {:unknown, [167]}
[10432]  {:unknown, 'B'}
[10433]  {:unknown, 'e'}
[10434]  {:unknown, 'c'}
[10435]  {:unknown, 'a'}
[10436]  {:unknown, 'u'}
[10437]  {:unknown, 's'}
[10438]  {:unknown, 'e'}
[10439]  {:unknown, ' '}
[10440]  {:unknown, 'i'}
[10441]  {:unknown, 't'}
[10442]  {:unknown, ' '}
[10443]  {:unknown, 'h'}
[10444]  {:unknown, 'a'}
[10445]  {:unknown, 's'}
[10446]  {:unknown, ' '}
[10447]  {:unknown, 'b'}
[10448]  {:unknown, 'e'}
[10449]  {:unknown, 'e'}
[10450]  {:unknown, 'n'}
[10451]  {:unknown, ' '}
[10452]  {:unknown, 's'}
[10453]  {:unknown, 'o'}
[10454]  {:unknown, ' '}
[10455]  {:unknown, 'w'}
[10456]  {:unknown, 'e'}
[10457]  {:unknown, 'l'}
[10458]  {:unknown, 'l'}
[10459]  {:unknown, '-'}
[10460]  {:unknown, 'p'}
[10461]  {:unknown, 'r'}
[10462]  {:unknown, 'o'}
[10463]  {:unknown, 't'}
[10464]  {:unknown, 'e'}
[10465]  {:unknown, 'c'}
[10466]  {:unknown, 't'}
[10467]  {:unknown, 'e'}
[10468]  {:unknown, 'd'}
[10469]  {:unknown, ','}
[10470]  {:unknown, ' '}
[10471]  {:unknown, 't'}
[10472]  {:unknown, 'h'}
[10473]  {:unknown, 'i'}
[10474]  {:unknown, 's'}
[10475]  {:unknown, ' '}
[10476]  {:unknown, 'r'}
[10477]  {:unknown, 'o'}
[10478]  {:unknown, 'o'}
[10479]  {:unknown, 'm'}
[10480]  {:unknown, ' '}
[10481]  {:unknown, 'h'}
[10482]  {:unknown, 'a'}
[10483]  {:unknown, 'r'}
[10484]  {:unknown, 'd'}
[10485]  {:unknown, 'l'}
[10486]  {:unknown, 'y'}
[10487]  {:unknown, ' '}
[10488]  {:unknown, 's'}
[10489]  {:unknown, 'h'}
[10490]  {:unknown, 'o'}
[10491]  {:unknown, 'w'}
[10492]  {:unknown, 's'}
[10493]  {:unknown, ' '}
[10494]  {:unknown, 's'}
[10495]  {:unknown, 'i'}
[10496]  {:unknown, 'g'}
[10497]  {:unknown, 'n'}
[10498]  {:unknown, 's'}
[10499]  {:unknown, ' '}
[10500]  {:unknown, 'o'}
[10501]  {:unknown, 'f'}
[10502]  {:unknown, ' '}
[10503]  {:unknown, 'd'}
[10504]  {:unknown, 'e'}
[10505]  {:unknown, 'c'}
[10506]  {:unknown, 'a'}
[10507]  {:unknown, 'y'}
[10508]  {:unknown, '.'}
[10509]  {:unknown, ' '}
[10510]  {:unknown, ' '}
[10511]  {:unknown, 'T'}
[10512]  {:unknown, 'h'}
[10513]  {:unknown, 'e'}
[10514]  {:unknown, ' '}
[10515]  {:unknown, 'w'}
[10516]  {:unknown, 'a'}
[10517]  {:unknown, 'l'}
[10518]  {:unknown, 'l'}
[10519]  {:unknown, 's'}
[10520]  {:unknown, ' '}
[10521]  {:unknown, 'a'}
[10522]  {:unknown, 'r'}
[10523]  {:unknown, 'e'}
[10524]  {:unknown, ' '}
[10525]  {:unknown, 'c'}
[10526]  {:unknown, 'o'}
[10527]  {:unknown, 'v'}
[10528]  {:unknown, 'e'}
[10529]  {:unknown, 'r'}
[10530]  {:unknown, 'e'}
[10531]  {:unknown, 'd'}
[10532]  {:unknown, ' '}
[10533]  {:unknown, 'i'}
[10534]  {:unknown, 'n'}
[10535]  {:unknown, ' '}
[10536]  {:unknown, 'e'}
[10537]  {:unknown, 'l'}
[10538]  {:unknown, 'a'}
[10539]  {:unknown, 'b'}
[10540]  {:unknown, 'o'}
[10541]  {:unknown, 'r'}
[10542]  {:unknown, 'a'}
[10543]  {:unknown, 't'}
[10544]  {:unknown, 'e'}
[10545]  {:unknown, ' '}
[10546]  {:unknown, 'm'}
[10547]  {:unknown, 'u'}
[10548]  {:unknown, 'r'}
[10549]  {:unknown, 'a'}
[10550]  {:unknown, 'l'}
[10551]  {:unknown, 's'}
[10552]  {:unknown, ' '}
[10553]  {:unknown, 'a'}
[10554]  {:unknown, 'n'}
[10555]  {:unknown, 'd'}
[10556]  {:unknown, ' '}
[10557]  {:unknown, 'd'}
[10558]  {:unknown, 'e'}
[10559]  {:unknown, 'c'}
[10560]  {:unknown, 'o'}
[10561]  {:unknown, 'r'}
[10562]  {:unknown, 'a'}
[10563]  {:unknown, 't'}
[10564]  {:unknown, 'e'}
[10565]  {:unknown, 'd'}
[10566]  {:unknown, ' '}
[10567]  {:unknown, 'w'}
[10568]  {:unknown, 'i'}
[10569]  {:unknown, 't'}
[10570]  {:unknown, 'h'}
[10571]  {:unknown, ' '}
[10572]  {:unknown, 'p'}
[10573]  {:unknown, 'r'}
[10574]  {:unknown, 'e'}
[10575]  {:unknown, 'c'}
[10576]  {:unknown, 'i'}
[10577]  {:unknown, 'o'}
[10578]  {:unknown, 'u'}
[10579]  {:unknown, 's'}
[10580]  {:unknown, ' '}
[10581]  {:unknown, 'm'}
[10582]  {:unknown, 'e'}
[10583]  {:unknown, 't'}
[10584]  {:unknown, 'a'}
[10585]  {:unknown, 'l'}
[10586]  {:unknown, 's'}
[10587]  {:unknown, ' '}
[10588]  {:unknown, 'a'}
[10589]  {:unknown, 'n'}
[10590]  {:unknown, 'd'}
[10591]  {:unknown, ' '}
[10592]  {:unknown, 's'}
[10593]  {:unknown, 't'}
[10594]  {:unknown, 'o'}
[10595]  {:unknown, 'n'}
[10596]  {:unknown, 'e'}
[10597]  {:unknown, 's'}
[10598]  {:unknown, '.'}
[10599]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[10603]  {:unknown, 't'}
[10604]  {:unknown, 'h'}
[10605]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[10609]  {:unknown, 'n'}
[10610]  {:unknown, 's'}
[10611]  {:unknown, [138]}
[10612]  {:unknown, 'Y'}
[10613]  {:unknown, 'o'}
[10614]  {:unknown, 'u'}
[10615]  {:unknown, ' '}
[10616]  {:unknown, 's'}
[10617]  {:unknown, 't'}
[10618]  {:unknown, 'a'}
[10619]  {:unknown, 'n'}
[10620]  {:unknown, 'd'}
[10621]  {:unknown, ' '}
[10622]  {:unknown, 'i'}
[10623]  {:unknown, 'n'}
[10624]  {:unknown, ' '}
[10625]  {:unknown, 'w'}
[10626]  {:unknown, 'h'}
[10627]  {:unknown, 'a'}
[10628]  {:unknown, 't'}
[10629]  {:unknown, ' '}
[10630]  {:unknown, 's'}
[10631]  {:unknown, 'e'}
[10632]  {:unknown, 'e'}
[10633]  {:unknown, 'm'}
[10634]  {:unknown, 's'}
[10635]  {:unknown, ' '}
[10636]  {:unknown, 't'}
[10637]  {:unknown, 'o'}
[10638]  {:unknown, ' '}
[10639]  {:unknown, 'h'}
[10640]  {:unknown, 'a'}
[10641]  {:unknown, 'v'}
[10642]  {:unknown, 'e'}
[10643]  {:unknown, ' '}
[10644]  {:unknown, 'o'}
[10645]  {:unknown, 'n'}
[10646]  {:unknown, 'c'}
[10647]  {:unknown, 'e'}
[10648]  {:unknown, ' '}
[10649]  {:unknown, 'b'}
[10650]  {:unknown, 'e'}
[10651]  {:unknown, 'e'}
[10652]  {:unknown, 'n'}
[10653]  {:unknown, ' '}
[10654]  {:unknown, 'a'}
[10655]  {:unknown, ' '}
[10656]  {:unknown, 'd'}
[10657]  {:unknown, 'i'}
[10658]  {:unknown, 'n'}
[10659]  {:unknown, 'i'}
[10660]  {:unknown, 'n'}
[10661]  {:unknown, 'g'}
[10662]  {:unknown, ' '}
[10663]  {:unknown, 'h'}
[10664]  {:unknown, 'a'}
[10665]  {:unknown, 'l'}
[10666]  {:unknown, 'l'}
[10667]  {:unknown, ';'}
[10668]  {:unknown, ' '}
[10669]  {:unknown, 'b'}
[10670]  {:unknown, 'r'}
[10671]  {:unknown, 'o'}
[10672]  {:unknown, 'k'}
[10673]  {:unknown, 'e'}
[10674]  {:unknown, 'n'}
[10675]  {:unknown, ' '}
[10676]  {:unknown, 't'}
[10677]  {:unknown, 'a'}
[10678]  {:unknown, 'b'}
[10679]  {:unknown, 'l'}
[10680]  {:unknown, 'e'}
[10681]  {:unknown, 's'}
[10682]  {:unknown, ' '}
[10683]  {:unknown, 'a'}
[10684]  {:unknown, 'n'}
[10685]  {:unknown, 'd'}
[10686]  {:unknown, ' '}
[10687]  {:unknown, 'p'}
[10688]  {:unknown, 'o'}
[10689]  {:unknown, 't'}
[10690]  {:unknown, 't'}
[10691]  {:unknown, 'e'}
[10692]  {:unknown, 'r'}
[10693]  {:unknown, 'y'}
[10694]  {:unknown, ' '}
[10695]  {:unknown, 'a'}
[10696]  {:unknown, 'r'}
[10697]  {:unknown, 'e'}
[10698]  {:unknown, ' '}
[10699]  {:unknown, 's'}
[10700]  {:unknown, 'c'}
[10701]  {:unknown, 'a'}
[10702]  {:unknown, 't'}
[10703]  {:unknown, 't'}
[10704]  {:unknown, 'e'}
[10705]  {:unknown, 'r'}
[10706]  {:unknown, 'e'}
[10707]  {:unknown, 'd'}
[10708]  {:unknown, ' '}
[10709]  {:unknown, 'e'}
[10710]  {:unknown, 'v'}
[10711]  {:unknown, 'e'}
[10712]  {:unknown, 'r'}
[10713]  {:unknown, 'y'}
[10714]  {:unknown, 'w'}
[10715]  {:unknown, 'h'}
[10716]  {:unknown, 'e'}
[10717]  {:unknown, 'r'}
[10718]  {:unknown, 'e'}
[10719]  {:unknown, '.'}
[10720]  {:unknown, ' '}
[10721]  {:unknown, ' '}
[10722]  {:unknown, 'A'}
[10723]  {:unknown, ' '}
[10724]  {:unknown, 's'}
[10725]  {:unknown, 't'}
[10726]  {:unknown, 'a'}
[10727]  {:unknown, 'i'}
[10728]  {:unknown, 'r'}
[10729]  {:unknown, 'c'}
[10730]  {:unknown, 'a'}
[10731]  {:unknown, 's'}
[10732]  {:unknown, 'e'}
[10733]  {:unknown, ' '}
[10734]  {:unknown, 'h'}
[10735]  {:unknown, 'e'}
[10736]  {:unknown, 'r'}
[10737]  {:unknown, 'e'}
[10738]  {:unknown, ' '}
[10739]  {:unknown, 'l'}
[10740]  {:unknown, 'e'}
[10741]  {:unknown, 'a'}
[10742]  {:unknown, 'd'}
[10743]  {:unknown, 's'}
[10744]  {:unknown, ' '}
[10745]  {:unknown, 'd'}
[10746]  {:unknown, 'o'}
[10747]  {:unknown, 'w'}
[10748]  {:unknown, 'n'}
[10749]  {:unknown, '.'}
[10750]  {:eq, {:value, 100}, {:value, 111}, {:value, 119}}
[10754]  {:unknown, 'n'}
[10755]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[10759]  {:unknown, 't'}
[10760]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[10764]  {:unknown, 'n'}
[10765]  {:unknown, 's'}
[10766]  {:unknown, 'z'}
[10767]  {:unknown, 'T'}
[10768]  {:unknown, 'h'}
[10769]  {:unknown, 'i'}
[10770]  {:unknown, 's'}
[10771]  {:unknown, ' '}
[10772]  {:unknown, 's'}
[10773]  {:unknown, 'e'}
[10774]  {:unknown, 'e'}
[10775]  {:unknown, 'm'}
[10776]  {:unknown, 's'}
[10777]  {:unknown, ' '}
[10778]  {:unknown, 't'}
[10779]  {:unknown, 'o'}
[10780]  {:unknown, ' '}
[10781]  {:unknown, 'b'}
[10782]  {:unknown, 'e'}
[10783]  {:unknown, ' '}
[10784]  {:unknown, 'a'}
[10785]  {:unknown, ' '}
[10786]  {:unknown, 'k'}
[10787]  {:unknown, 'i'}
[10788]  {:unknown, 't'}
[10789]  {:unknown, 'c'}
[10790]  {:unknown, 'h'}
[10791]  {:unknown, 'e'}
[10792]  {:unknown, 'n'}
[10793]  {:unknown, ';'}
[10794]  {:unknown, ' '}
[10795]  {:unknown, 't'}
[10796]  {:unknown, 'h'}
[10797]  {:unknown, 'e'}
[10798]  {:unknown, 'r'}
[10799]  {:unknown, 'e'}
[10800]  {:unknown, ' '}
[10801]  {:unknown, 'a'}
[10802]  {:unknown, 'r'}
[10803]  {:unknown, 'e'}
[10804]  {:unknown, ' '}
[10805]  {:unknown, 'b'}
[10806]  {:unknown, 'r'}
[10807]  {:unknown, 'i'}
[10808]  {:unknown, 'c'}
[10809]  {:unknown, 'k'}
[10810]  {:unknown, ' '}
[10811]  {:unknown, 's'}
[10812]  {:unknown, 't'}
[10813]  {:unknown, 'o'}
[10814]  {:unknown, 'v'}
[10815]  {:unknown, 'e'}
[10816]  {:unknown, 's'}
[10817]  {:unknown, ' '}
[10818]  {:unknown, 'a'}
[10819]  {:unknown, 'n'}
[10820]  {:unknown, 'd'}
[10821]  {:unknown, ' '}
[10822]  {:unknown, 's'}
[10823]  {:unknown, 'h'}
[10824]  {:unknown, 'e'}
[10825]  {:unknown, 'l'}
[10826]  {:unknown, 'v'}
[10827]  {:unknown, 'e'}
[10828]  {:unknown, 's'}
[10829]  {:unknown, ' '}
[10830]  {:unknown, 'a'}
[10831]  {:unknown, 'l'}
[10832]  {:unknown, 'o'}
[10833]  {:unknown, 'n'}
[10834]  {:unknown, 'g'}
[10835]  {:unknown, ' '}
[10836]  {:unknown, 't'}
[10837]  {:unknown, 'h'}
[10838]  {:unknown, 'e'}
[10839]  {:unknown, ' '}
[10840]  {:unknown, 'w'}
[10841]  {:unknown, 'a'}
[10842]  {:unknown, 'l'}
[10843]  {:unknown, 'l'}
[10844]  {:unknown, '.'}
[10845]  {:unknown, ' '}
[10846]  {:unknown, ' '}
[10847]  {:unknown, 'E'}
[10848]  {:unknown, 'v'}
[10849]  {:unknown, 'e'}
[10850]  {:unknown, 'r'}
[10851]  {:unknown, 'y'}
[10852]  {:unknown, 't'}
[10853]  {:unknown, 'h'}
[10854]  {:unknown, 'i'}
[10855]  {:unknown, 'n'}
[10856]  {:unknown, 'g'}
[10857]  {:unknown, ' '}
[10858]  {:unknown, 'h'}
[10859]  {:unknown, 'e'}
[10860]  {:unknown, 'r'}
[10861]  {:unknown, 'e'}
[10862]  {:unknown, ' '}
[10863]  {:unknown, 'h'}
[10864]  {:unknown, 'a'}
[10865]  {:unknown, 's'}
[10866]  {:unknown, ' '}
[10867]  {:unknown, 'f'}
[10868]  {:unknown, 'a'}
[10869]  {:unknown, 'l'}
[10870]  {:unknown, 'l'}
[10871]  {:unknown, 'e'}
[10872]  {:unknown, 'n'}
[10873]  {:unknown, ' '}
[10874]  {:unknown, 'i'}
[10875]  {:unknown, 'n'}
[10876]  {:unknown, 't'}
[10877]  {:unknown, 'o'}
[10878]  {:unknown, ' '}
[10879]  {:unknown, 'd'}
[10880]  {:unknown, 'i'}
[10881]  {:unknown, 's'}
[10882]  {:unknown, 'r'}
[10883]  {:unknown, 'e'}
[10884]  {:unknown, 'p'}
[10885]  {:unknown, 'a'}
[10886]  {:unknown, 'i'}
[10887]  {:unknown, 'r'}
[10888]  {:unknown, '.'}
[10889]  {:push, {:value, 117}}
[10891]  {:unknown, 'p'}
[10892]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[10896]  {:unknown, 'n'}
[10897]  {:unknown, 's'}
[10898]  {:unknown, [179]}
[10899]  {:unknown, 'Y'}
[10900]  {:unknown, 'o'}
[10901]  {:unknown, 'u'}
[10902]  {:unknown, ' '}
[10903]  {:unknown, 'f'}
[10904]  {:unknown, 'i'}
[10905]  {:unknown, 'n'}
[10906]  {:unknown, 'd'}
[10907]  {:unknown, ' '}
[10908]  {:unknown, 'y'}
[10909]  {:unknown, 'o'}
[10910]  {:unknown, 'u'}
[10911]  {:unknown, 'r'}
[10912]  {:unknown, 's'}
[10913]  {:unknown, 'e'}
[10914]  {:unknown, 'l'}
[10915]  {:unknown, 'f'}
[10916]  {:unknown, ' '}
[10917]  {:unknown, 'i'}
[10918]  {:unknown, 'n'}
[10919]  {:unknown, ' '}
[10920]  {:unknown, 'w'}
[10921]  {:unknown, 'h'}
[10922]  {:unknown, 'a'}
[10923]  {:unknown, 't'}
[10924]  {:unknown, ' '}
[10925]  {:unknown, 'w'}
[10926]  {:unknown, 'a'}
[10927]  {:unknown, 's'}
[10928]  {:unknown, ' '}
[10929]  {:unknown, 'o'}
[10930]  {:unknown, 'n'}
[10931]  {:unknown, 'c'}
[10932]  {:unknown, 'e'}
[10933]  {:unknown, ' '}
[10934]  {:unknown, 't'}
[10935]  {:unknown, 'h'}
[10936]  {:unknown, 'e'}
[10937]  {:unknown, ' '}
[10938]  {:unknown, 'l'}
[10939]  {:unknown, 'i'}
[10940]  {:unknown, 'v'}
[10941]  {:unknown, 'i'}
[10942]  {:unknown, 'n'}
[10943]  {:unknown, 'g'}
[10944]  {:unknown, ' '}
[10945]  {:unknown, 'q'}
[10946]  {:unknown, 'u'}
[10947]  {:unknown, 'a'}
[10948]  {:unknown, 'r'}
[10949]  {:unknown, 't'}
[10950]  {:unknown, 'e'}
[10951]  {:unknown, 'r'}
[10952]  {:unknown, 's'}
[10953]  {:unknown, ' '}
[10954]  {:unknown, 'f'}
[10955]  {:unknown, 'o'}
[10956]  {:unknown, 'r'}
[10957]  {:unknown, ' '}
[10958]  {:unknown, 't'}
[10959]  {:unknown, 'h'}
[10960]  {:unknown, 'e'}
[10961]  {:unknown, ' '}
[10962]  {:unknown, 'c'}
[10963]  {:unknown, 'o'}
[10964]  {:unknown, 'm'}
[10965]  {:unknown, 'p'}
[10966]  {:unknown, 'l'}
[10967]  {:unknown, 'e'}
[10968]  {:unknown, 'x'}
[10969]  {:unknown, '.'}
[10970]  {:unknown, ' '}
[10971]  {:unknown, ' '}
[10972]  {:unknown, 'M'}
[10973]  {:unknown, 'a'}
[10974]  {:unknown, 'n'}
[10975]  {:unknown, 'y'}
[10976]  {:unknown, ' '}
[10977]  {:unknown, 's'}
[10978]  {:unknown, 'm'}
[10979]  {:unknown, 'a'}
[10980]  {:unknown, 'l'}
[10981]  {:unknown, 'l'}
[10982]  {:unknown, 'e'}
[10983]  {:unknown, 'r'}
[10984]  {:unknown, ' '}
[10985]  {:unknown, 'r'}
[10986]  {:unknown, 'o'}
[10987]  {:unknown, 'o'}
[10988]  {:unknown, 'm'}
[10989]  {:unknown, 's'}
[10990]  {:unknown, ' '}
[10991]  {:unknown, 'w'}
[10992]  {:unknown, 'h'}
[10993]  {:unknown, 'i'}
[10994]  {:unknown, 'c'}
[10995]  {:unknown, 'h'}
[10996]  {:unknown, ' '}
[10997]  {:unknown, 'o'}
[10998]  {:unknown, 'n'}
[10999]  {:unknown, 'c'}
[11000]  {:unknown, 'e'}
[11001]  {:unknown, ' '}
[11002]  {:unknown, 'h'}
[11003]  {:unknown, 'a'}
[11004]  {:unknown, 'd'}
[11005]  {:unknown, ' '}
[11006]  {:unknown, 'w'}
[11007]  {:unknown, 'a'}
[11008]  {:unknown, 'l'}
[11009]  {:unknown, 'l'}
[11010]  {:unknown, 's'}
[11011]  {:unknown, ' '}
[11012]  {:unknown, 't'}
[11013]  {:unknown, 'o'}
[11014]  {:unknown, ' '}
[11015]  {:unknown, 'd'}
[11016]  {:unknown, 'i'}
[11017]  {:unknown, 'v'}
[11018]  {:unknown, 'i'}
[11019]  {:unknown, 'd'}
[11020]  {:unknown, 'e'}
[11021]  {:unknown, ' '}
[11022]  {:unknown, 't'}
[11023]  {:unknown, 'h'}
[11024]  {:unknown, 'e'}
[11025]  {:unknown, 'm'}
[11026]  {:unknown, ' '}
[11027]  {:unknown, 'n'}
[11028]  {:unknown, 'o'}
[11029]  {:unknown, 'w'}
[11030]  {:unknown, ' '}
[11031]  {:unknown, 'l'}
[11032]  {:unknown, 'a'}
[11033]  {:unknown, 'y'}
[11034]  {:unknown, ' '}
[11035]  {:unknown, 'i'}
[11036]  {:unknown, 'n'}
[11037]  {:unknown, ' '}
[11038]  {:unknown, 'd'}
[11039]  {:unknown, 'i'}
[11040]  {:unknown, 's'}
[11041]  {:unknown, 'a'}
[11042]  {:unknown, 'r'}
[11043]  {:unknown, 'r'}
[11044]  {:unknown, 'a'}
[11045]  {:unknown, 'y'}
[11046]  {:unknown, '.'}
[11047]  {:unknown, ' '}
[11048]  {:unknown, ' '}
[11049]  {:unknown, 'T'}
[11050]  {:unknown, 'h'}
[11051]  {:unknown, 'e'}
[11052]  {:unknown, 'r'}
[11053]  {:unknown, 'e'}
[11054]  {:unknown, ' '}
[11055]  {:unknown, 'i'}
[11056]  {:unknown, 's'}
[11057]  {:unknown, ' '}
[11058]  {:unknown, 'a'}
[11059]  {:unknown, ' '}
[11060]  {:unknown, 's'}
[11061]  {:unknown, 't'}
[11062]  {:unknown, 'a'}
[11063]  {:unknown, 'i'}
[11064]  {:unknown, 'r'}
[11065]  {:unknown, 'c'}
[11066]  {:unknown, 'a'}
[11067]  {:unknown, 's'}
[11068]  {:unknown, 'e'}
[11069]  {:unknown, ' '}
[11070]  {:unknown, 'u'}
[11071]  {:unknown, 'p'}
[11072]  {:unknown, ' '}
[11073]  {:unknown, 'h'}
[11074]  {:unknown, 'e'}
[11075]  {:unknown, 'r'}
[11076]  {:unknown, 'e'}
[11077]  {:unknown, '.'}
[11078]  {:push, {:value, 117}}
[11080]  {:unknown, 'p'}
[11081]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[11085]  {:unknown, 't'}
[11086]  {:gt, {:value, 82}, {:value, 117}, {:value, 105}}
[11090]  {:unknown, 'n'}
[11091]  {:unknown, 's'}
[11092]  {:unknown, [169]}
[11093]  {:unknown, 'T'}
[11094]  {:unknown, 'h'}
[11095]  {:unknown, 'i'}
[11096]  {:unknown, 's'}
[11097]  {:unknown, ' '}
[11098]  {:unknown, 'w'}
[11099]  {:unknown, 'a'}
[11100]  {:unknown, 's'}
[11101]  {:unknown, ' '}
[11102]  {:unknown, 'l'}
[11103]  {:unknown, 'o'}
[11104]  {:unknown, 'n'}
[11105]  {:unknown, 'g'}
[11106]  {:unknown, ' '}
[11107]  {:unknown, 'a'}
[11108]  {:unknown, 'g'}
[11109]  {:unknown, 'o'}
[11110]  {:unknown, ' '}
[11111]  {:unknown, 'a'}
[11112]  {:unknown, ' '}
[11113]  {:unknown, 'l'}
[11114]  {:unknown, 'a'}
[11115]  {:unknown, 'v'}
[11116]  {:unknown, 'i'}
[11117]  {:unknown, 's'}
[11118]  {:unknown, 'h'}
[11119]  {:unknown, ' '}
[11120]  {:unknown, 't'}
[11121]  {:unknown, 'h'}
[11122]  {:unknown, 'r'}
[11123]  {:unknown, 'o'}
[11124]  {:unknown, 'n'}
[11125]  {:unknown, 'e'}
[11126]  {:unknown, ' '}
[11127]  {:unknown, 'r'}
[11128]  {:unknown, 'o'}
[11129]  {:unknown, 'o'}
[11130]  {:unknown, 'm'}
[11131]  {:unknown, '.'}
[11132]  {:unknown, ' '}
[11133]  {:unknown, ' '}
[11134]  {:unknown, 'D'}
[11135]  {:unknown, 'r'}
[11136]  {:unknown, 'i'}
[11137]  {:unknown, 'e'}
[11138]  {:unknown, 'd'}
[11139]  {:unknown, '-'}
[11140]  {:unknown, 'u'}
[11141]  {:unknown, 'p'}
[11142]  {:unknown, ' '}
[11143]  {:unknown, 'f'}
[11144]  {:unknown, 'o'}
[11145]  {:unknown, 'u'}
[11146]  {:unknown, 'n'}
[11147]  {:unknown, 't'}
[11148]  {:unknown, 'a'}
[11149]  {:unknown, 'i'}
[11150]  {:unknown, 'n'}
[11151]  {:unknown, 's'}
[11152]  {:unknown, ' '}
[11153]  {:unknown, 'a'}
[11154]  {:unknown, 'n'}
[11155]  {:unknown, 'd'}
[11156]  {:unknown, ' '}
[11157]  {:unknown, 'c'}
[11158]  {:unknown, 'r'}
[11159]  {:unknown, 'u'}
[11160]  {:unknown, 'm'}
[11161]  {:unknown, 'b'}
[11162]  {:unknown, 'l'}
[11163]  {:unknown, 'i'}
[11164]  {:unknown, 'n'}
[11165]  {:unknown, 'g'}
[11166]  {:unknown, ' '}
[11167]  {:unknown, 's'}
[11168]  {:unknown, 't'}
[11169]  {:unknown, 'a'}
[11170]  {:unknown, 't'}
[11171]  {:unknown, 'u'}
[11172]  {:unknown, 'e'}
[11173]  {:unknown, 's'}
[11174]  {:unknown, ' '}
[11175]  {:unknown, 'l'}
[11176]  {:unknown, 'i'}
[11177]  {:unknown, 'n'}
[11178]  {:unknown, 'e'}
[11179]  {:unknown, ' '}
[11180]  {:unknown, 't'}
[11181]  {:unknown, 'h'}
[11182]  {:unknown, 'e'}
[11183]  {:unknown, ' '}
[11184]  {:unknown, 'w'}
[11185]  {:unknown, 'a'}
[11186]  {:unknown, 'l'}
[11187]  {:unknown, 'l'}
[11188]  {:unknown, 's'}
[11189]  {:unknown, ','}
[11190]  {:unknown, ' '}
[11191]  {:unknown, 'a'}
[11192]  {:unknown, 'n'}
[11193]  {:unknown, 'd'}
[11194]  {:unknown, ' '}
[11195]  {:unknown, 't'}
[11196]  {:unknown, 'h'}
[11197]  {:unknown, 'e'}
[11198]  {:unknown, ' '}
[11199]  {:unknown, 'c'}
[11200]  {:unknown, 'a'}
[11201]  {:unknown, 'r'}
[11202]  {:unknown, 'v'}
[11203]  {:unknown, 'e'}
[11204]  {:unknown, 'd'}
[11205]  {:unknown, ' '}
[11206]  {:unknown, 's'}
[11207]  {:unknown, 't'}
[11208]  {:unknown, 'o'}
[11209]  {:unknown, 'n'}
[11210]  {:unknown, 'e'}
[11211]  {:unknown, ' '}
[11212]  {:unknown, 't'}
[11213]  {:unknown, 'h'}
[11214]  {:unknown, 'r'}
[11215]  {:unknown, 'o'}
[11216]  {:unknown, 'n'}
[11217]  {:unknown, 'e'}
[11218]  {:unknown, ' '}
[11219]  {:unknown, 'i'}
[11220]  {:unknown, 'n'}
[11221]  {:unknown, ' '}
[11222]  {:unknown, 't'}
[11223]  {:unknown, 'h'}
[11224]  {:unknown, 'e'}
[11225]  {:unknown, ' '}
[11226]  {:unknown, 'c'}
[11227]  {:unknown, 'e'}
[11228]  {:unknown, 'n'}
[11229]  {:unknown, 't'}
[11230]  {:unknown, 'e'}
[11231]  {:unknown, 'r'}
[11232]  {:unknown, ' '}
[11233]  {:unknown, 'o'}
[11234]  {:unknown, 'f'}
[11235]  {:unknown, ' '}
[11236]  {:unknown, 't'}
[11237]  {:unknown, 'h'}
[11238]  {:unknown, 'e'}
[11239]  {:unknown, ' '}
[11240]  {:unknown, 'r'}
[11241]  {:unknown, 'o'}
[11242]  {:unknown, 'o'}
[11243]  {:unknown, 'm'}
[11244]  {:unknown, ' '}
[11245]  {:unknown, 'i'}
[11246]  {:unknown, 's'}
[11247]  {:unknown, ' '}
[11248]  {:unknown, 'f'}
[11249]  {:unknown, 'a'}
[11250]  {:unknown, 'l'}
[11251]  {:unknown, 'l'}
[11252]  {:unknown, 'i'}
[11253]  {:unknown, 'n'}
[11254]  {:unknown, 'g'}
[11255]  {:unknown, ' '}
[11256]  {:unknown, 'a'}
[11257]  {:unknown, 'p'}
[11258]  {:unknown, 'a'}
[11259]  {:unknown, 'r'}
[11260]  {:unknown, 't'}
[11261]  {:unknown, '.'}
[11262]  {:eq, {:value, 100}, {:value, 111}, {:value, 119}}
[11266]  {:unknown, 'n'}
[11267]  {:in, {:value, 83}}
[11269]  {:unknown, 'y'}
[11270]  {:unknown, 'n'}
[11271]  {:unknown, 'a'}
[11272]  {:unknown, 'c'}
[11273]  {:unknown, 'o'}
[11274]  {:unknown, 'r'}
[11275]  {:unknown, ' '}
[11276]  {:unknown, 'H'}
[11277]  {:unknown, 'e'}
[11278]  {:unknown, 'a'}
[11279]  {:unknown, 'd'}
[11280]  {:unknown, 'q'}
[11281]  {:unknown, 'u'}
[11282]  {:unknown, 'a'}
[11283]  {:unknown, 'r'}
[11284]  {:unknown, 't'}
[11285]  {:unknown, 'e'}
[11286]  {:unknown, 'r'}
[11287]  {:unknown, 's'}
[11288]  {:unknown, [255]}
[11289]  {:unknown, 'Y'}
[11290]  {:unknown, 'o'}
[11291]  {:unknown, 'u'}
[11292]  {:unknown, ' '}
[11293]  {:unknown, 's'}
[11294]  {:unknown, 't'}
[11295]  {:unknown, 'a'}
[11296]  {:unknown, 'n'}
[11297]  {:unknown, 'd'}
[11298]  {:unknown, ' '}
[11299]  {:unknown, 'i'}
[11300]  {:unknown, 'n'}
[11301]  {:unknown, ' '}
[11302]  {:unknown, 't'}
[11303]  {:unknown, 'h'}
[11304]  {:unknown, 'e'}
[11305]  {:unknown, ' '}
[11306]  {:unknown, 'l'}
[11307]  {:unknown, 'o'}
[11308]  {:unknown, 'b'}
[11309]  {:unknown, 'b'}
[11310]  {:unknown, 'y'}
[11311]  {:unknown, ' '}
[11312]  {:unknown, 'o'}
[11313]  {:unknown, 'f'}
[11314]  {:unknown, ' '}
[11315]  {:unknown, 'w'}
[11316]  {:unknown, 'h'}
[11317]  {:unknown, 'a'}
[11318]  {:unknown, 't'}
[11319]  {:unknown, ' '}
[11320]  {:unknown, 'a'}
[11321]  {:unknown, 'p'}
[11322]  {:unknown, 'p'}
[11323]  {:unknown, 'e'}
[11324]  {:unknown, 'a'}
[11325]  {:unknown, 'r'}
[11326]  {:unknown, 's'}
[11327]  {:unknown, ' '}
[11328]  {:unknown, 't'}
[11329]  {:unknown, 'o'}
[11330]  {:unknown, ' '}
[11331]  {:unknown, 'b'}
[11332]  {:unknown, 'e'}
[11333]  {:unknown, ' '}
[11334]  {:unknown, 'a'}
[11335]  {:unknown, ' '}
[11336]  {:unknown, 'r'}
[11337]  {:unknown, 'e'}
[11338]  {:unknown, 'a'}
[11339]  {:unknown, 'l'}
[11340]  {:unknown, 'l'}
[11341]  {:unknown, 'y'}
[11342]  {:unknown, ' '}
[11343]  {:unknown, 'f'}
[11344]  {:unknown, 'u'}
[11345]  {:unknown, 'n'}
[11346]  {:unknown, ' '}
[11347]  {:unknown, 'p'}
[11348]  {:unknown, 'l'}
[11349]  {:unknown, 'a'}
[11350]  {:unknown, 'c'}
[11351]  {:unknown, 'e'}
[11352]  {:unknown, ' '}
[11353]  {:unknown, 't'}
[11354]  {:unknown, 'o'}
[11355]  {:unknown, ' '}
[11356]  {:unknown, 'w'}
[11357]  {:unknown, 'o'}
[11358]  {:unknown, 'r'}
[11359]  {:unknown, 'k'}
[11360]  {:unknown, '!'}
[11361]  {:unknown, ' '}
[11362]  {:unknown, ' '}
[11363]  {:unknown, 'S'}
[11364]  {:unknown, 'a'}
[11365]  {:unknown, 'd'}
[11366]  {:unknown, 'l'}
[11367]  {:unknown, 'y'}
[11368]  {:unknown, ','}
[11369]  {:unknown, ' '}
[11370]  {:unknown, 't'}
[11371]  {:unknown, 'h'}
[11372]  {:unknown, 'e'}
[11373]  {:unknown, 'r'}
[11374]  {:unknown, 'e'}
[11375]  {:unknown, ' '}
[11376]  {:unknown, 'd'}
[11377]  {:unknown, 'o'}
[11378]  {:unknown, 'e'}
[11379]  {:unknown, 's'}
[11380]  {:unknown, 'n'}
[11381]  {:unknown, '\''}
[11382]  {:unknown, 't'}
[11383]  {:unknown, ' '}
[11384]  {:unknown, 's'}
[11385]  {:unknown, 'e'}
[11386]  {:unknown, 'e'}
[11387]  {:unknown, 'm'}
[11388]  {:unknown, ' '}
[11389]  {:unknown, 't'}
[11390]  {:unknown, 'o'}
[11391]  {:unknown, ' '}
[11392]  {:unknown, 'b'}
[11393]  {:unknown, 'e'}
[11394]  {:unknown, ' '}
[11395]  {:unknown, 'a'}
[11396]  {:unknown, 'n'}
[11397]  {:unknown, 'y'}
[11398]  {:unknown, 'o'}
[11399]  {:unknown, 'n'}
[11400]  {:unknown, 'e'}
[11401]  {:unknown, ' '}
[11402]  {:unknown, 'a'}
[11403]  {:unknown, 'r'}
[11404]  {:unknown, 'o'}
[11405]  {:unknown, 'u'}
[11406]  {:unknown, 'n'}
[11407]  {:unknown, 'd'}
[11408]  {:unknown, ' '}
[11409]  {:unknown, 'a'}
[11410]  {:unknown, 't'}
[11411]  {:unknown, ' '}
[11412]  {:unknown, 't'}
[11413]  {:unknown, 'h'}
[11414]  {:unknown, 'e'}
[11415]  {:unknown, ' '}
[11416]  {:unknown, 'm'}
[11417]  {:unknown, 'o'}
[11418]  {:unknown, 'm'}
[11419]  {:unknown, 'e'}
[11420]  {:unknown, 'n'}
[11421]  {:unknown, 't'}
[11422]  {:unknown, ','}
[11423]  {:unknown, ' '}
[11424]  {:unknown, 's'}
[11425]  {:unknown, 'o'}
[11426]  {:unknown, ' '}
[11427]  {:unknown, 'y'}
[11428]  {:unknown, 'o'}
[11429]  {:unknown, 'u'}
[11430]  {:unknown, ' '}
[11431]  {:unknown, 'm'}
[11432]  {:unknown, 'a'}
[11433]  {:unknown, 'k'}
[11434]  {:unknown, 'e'}
[11435]  {:unknown, ' '}
[11436]  {:unknown, 'a'}
[11437]  {:unknown, ' '}
[11438]  {:unknown, 'n'}
[11439]  {:unknown, 'o'}
[11440]  {:unknown, 't'}
[11441]  {:unknown, 'e'}
[11442]  {:unknown, ' '}
[11443]  {:unknown, 't'}
[11444]  {:unknown, 'o'}
[11445]  {:unknown, ' '}
[11446]  {:unknown, 'c'}
[11447]  {:unknown, 'a'}
[11448]  {:unknown, 'l'}
[11449]  {:unknown, 'l'}
[11450]  {:unknown, ' '}
[11451]  {:unknown, 't'}
[11452]  {:unknown, 'h'}
[11453]  {:unknown, 'e'}
[11454]  {:unknown, 'm'}
[11455]  {:unknown, ' '}
[11456]  {:unknown, 'l'}
[11457]  {:unknown, 'a'}
[11458]  {:unknown, 't'}
[11459]  {:unknown, 'e'}
[11460]  {:unknown, 'r'}
[11461]  {:unknown, '.'}
[11462]  {:unknown, ' '}
[11463]  {:unknown, ' '}
[11464]  {:unknown, 'T'}
[11465]  {:unknown, 'h'}
[11466]  {:unknown, 'e'}
[11467]  {:unknown, ' '}
[11468]  {:unknown, 'b'}
[11469]  {:unknown, 'o'}
[11470]  {:unknown, 'o'}
[11471]  {:unknown, 'k'}
[11472]  {:unknown, 's'}
[11473]  {:unknown, 'h'}
[11474]  {:unknown, 'e'}
[11475]  {:unknown, 'l'}
[11476]  {:unknown, 'f'}
[11477]  {:unknown, ' '}
[11478]  {:unknown, 'h'}
[11479]  {:unknown, 'e'}
[11480]  {:unknown, 'r'}
[11481]  {:unknown, 'e'}
[11482]  {:unknown, ' '}
[11483]  {:unknown, 'l'}
[11484]  {:unknown, 'o'}
[11485]  {:unknown, 'o'}
[11486]  {:unknown, 'k'}
[11487]  {:unknown, 's'}
[11488]  {:unknown, ' '}
[11489]  {:unknown, 'l'}
[11490]  {:unknown, 'i'}
[11491]  {:unknown, 'k'}
[11492]  {:unknown, 'e'}
[11493]  {:unknown, ' '}
[11494]  {:unknown, 'i'}
[11495]  {:unknown, 't'}
[11496]  {:unknown, ' '}
[11497]  {:unknown, 'm'}
[11498]  {:unknown, 'i'}
[11499]  {:unknown, 'g'}
[11500]  {:unknown, 'h'}
[11501]  {:unknown, 't'}
[11502]  {:unknown, ' '}
[11503]  {:unknown, 'h'}
[11504]  {:unknown, 'a'}
[11505]  {:unknown, 'v'}
[11506]  {:unknown, 'e'}
[11507]  {:unknown, ' '}
[11508]  {:unknown, 's'}
[11509]  {:unknown, 'o'}
[11510]  {:unknown, 'm'}
[11511]  {:unknown, 'e'}
[11512]  {:unknown, 't'}
[11513]  {:unknown, 'h'}
[11514]  {:unknown, 'i'}
[11515]  {:unknown, 'n'}
[11516]  {:unknown, 'g'}
[11517]  {:unknown, ' '}
[11518]  {:unknown, 'i'}
[11519]  {:unknown, 'n'}
[11520]  {:unknown, 't'}
[11521]  {:unknown, 'e'}
[11522]  {:unknown, 'r'}
[11523]  {:unknown, 'e'}
[11524]  {:unknown, 's'}
[11525]  {:unknown, 't'}
[11526]  {:unknown, 'i'}
[11527]  {:unknown, 'n'}
[11528]  {:unknown, 'g'}
[11529]  {:unknown, ' '}
[11530]  {:unknown, 'i'}
[11531]  {:unknown, 'n'}
[11532]  {:unknown, ' '}
[11533]  {:unknown, 'i'}
[11534]  {:unknown, 't'}
[11535]  {:unknown, ','}
[11536]  {:unknown, ' '}
[11537]  {:unknown, 't'}
[11538]  {:unknown, 'h'}
[11539]  {:unknown, 'o'}
[11540]  {:unknown, 'u'}
[11541]  {:unknown, 'g'}
[11542]  {:unknown, 'h'}
[11543]  {:unknown, '.'}
[11544]  {:jt, {:value, 111}, {:value, 117}}
[11547]  {:unknown, 't'}
[11548]  {:unknown, 's'}
[11549]  {:unknown, 'i'}
[11550]  {:unknown, 'd'}
[11551]  {:unknown, 'e'}
[11552]  {:in, {:value, 83}}
[11554]  {:unknown, 'y'}
[11555]  {:unknown, 'n'}
[11556]  {:unknown, 'a'}
[11557]  {:unknown, 'c'}
[11558]  {:unknown, 'o'}
[11559]  {:unknown, 'r'}
[11560]  {:unknown, ' '}
[11561]  {:unknown, 'H'}
[11562]  {:unknown, 'e'}
[11563]  {:unknown, 'a'}
[11564]  {:unknown, 'd'}
[11565]  {:unknown, 'q'}
[11566]  {:unknown, 'u'}
[11567]  {:unknown, 'a'}
[11568]  {:unknown, 'r'}
[11569]  {:unknown, 't'}
[11570]  {:unknown, 'e'}
[11571]  {:unknown, 'r'}
[11572]  {:unknown, 's'}
[11573]  {:unknown, [145]}
[11574]  {:unknown, 'I'}
[11575]  {:unknown, 't'}
[11576]  {:unknown, '\''}
[11577]  {:unknown, 's'}
[11578]  {:unknown, ' '}
[11579]  {:unknown, 'a'}
[11580]  {:unknown, ' '}
[11581]  {:unknown, 'w'}
[11582]  {:unknown, 'a'}
[11583]  {:unknown, 'r'}
[11584]  {:unknown, 'm'}
[11585]  {:unknown, ','}
[11586]  {:unknown, ' '}
[11587]  {:unknown, 's'}
[11588]  {:unknown, 'u'}
[11589]  {:unknown, 'n'}
[11590]  {:unknown, 'n'}
[11591]  {:unknown, 'y'}
[11592]  {:unknown, ' '}
[11593]  {:unknown, 'd'}
[11594]  {:unknown, 'a'}
[11595]  {:unknown, 'y'}
[11596]  {:unknown, '!'}
[11597]  {:unknown, ' '}
[11598]  {:unknown, ' '}
[11599]  {:unknown, 'W'}
[11600]  {:unknown, 'h'}
[11601]  {:unknown, 'i'}
[11602]  {:unknown, 'l'}
[11603]  {:unknown, 'e'}
[11604]  {:unknown, ' '}
[11605]  {:unknown, 't'}
[11606]  {:unknown, 'h'}
[11607]  {:unknown, 'e'}
[11608]  {:unknown, ' '}
[11609]  {:unknown, 'b'}
[11610]  {:unknown, 'r'}
[11611]  {:unknown, 'e'}
[11612]  {:unknown, 'e'}
[11613]  {:unknown, 'z'}
[11614]  {:unknown, 'e'}
[11615]  {:unknown, ' '}
[11616]  {:unknown, 'f'}
[11617]  {:unknown, 'r'}
[11618]  {:unknown, 'o'}
[11619]  {:unknown, 'm'}
[11620]  {:unknown, ' '}
[11621]  {:unknown, 'L'}
[11622]  {:unknown, 'a'}
[11623]  {:unknown, 'k'}
[11624]  {:unknown, 'e'}
[11625]  {:unknown, ' '}
[11626]  {:unknown, 'E'}
[11627]  {:unknown, 'r'}
[11628]  {:unknown, 'i'}
[11629]  {:unknown, 'e'}
[11630]  {:unknown, ' '}
[11631]  {:unknown, 'i'}
[11632]  {:unknown, 's'}
[11633]  {:unknown, ' '}
[11634]  {:unknown, 'c'}
[11635]  {:unknown, 'e'}
[11636]  {:unknown, 'r'}
[11637]  {:unknown, 't'}
[11638]  {:unknown, 'a'}
[11639]  {:unknown, 'i'}
[11640]  {:unknown, 'n'}
[11641]  {:unknown, 'l'}
[11642]  {:unknown, 'y'}
[11643]  {:unknown, ' '}
[11644]  {:unknown, 'r'}
[11645]  {:unknown, 'e'}
[11646]  {:unknown, 'f'}
[11647]  {:unknown, 'r'}
[11648]  {:unknown, 'e'}
[11649]  {:unknown, 's'}
[11650]  {:unknown, 'h'}
[11651]  {:unknown, 'i'}
[11652]  {:unknown, 'n'}
[11653]  {:unknown, 'g'}
[11654]  {:unknown, ','}
[11655]  {:unknown, ' '}
[11656]  {:unknown, 'y'}
[11657]  {:unknown, 'o'}
[11658]  {:unknown, 'u'}
[11659]  {:unknown, ' '}
[11660]  {:unknown, 'd'}
[11661]  {:unknown, 'o'}
[11662]  {:unknown, 'n'}
[11663]  {:unknown, '\''}
[11664]  {:unknown, 't'}
[11665]  {:unknown, ' '}
[11666]  {:unknown, 's'}
[11667]  {:unknown, 'e'}
[11668]  {:unknown, 'e'}
[11669]  {:unknown, ' '}
[11670]  {:unknown, 'a'}
[11671]  {:unknown, 'n'}
[11672]  {:unknown, 'y'}
[11673]  {:unknown, 't'}
[11674]  {:unknown, 'h'}
[11675]  {:unknown, 'i'}
[11676]  {:unknown, 'n'}
[11677]  {:unknown, 'g'}
[11678]  {:unknown, ' '}
[11679]  {:unknown, 'h'}
[11680]  {:unknown, 'e'}
[11681]  {:unknown, 'r'}
[11682]  {:unknown, 'e'}
[11683]  {:unknown, ' '}
[11684]  {:unknown, 't'}
[11685]  {:unknown, 'h'}
[11686]  {:unknown, 'a'}
[11687]  {:unknown, 't'}
[11688]  {:unknown, ' '}
[11689]  {:unknown, 'w'}
[11690]  {:unknown, 'i'}
[11691]  {:unknown, 'l'}
[11692]  {:unknown, 'l'}
[11693]  {:unknown, ' '}
[11694]  {:unknown, 'h'}
[11695]  {:unknown, 'e'}
[11696]  {:unknown, 'l'}
[11697]  {:unknown, 'p'}
[11698]  {:unknown, ' '}
[11699]  {:unknown, 'y'}
[11700]  {:unknown, 'o'}
[11701]  {:unknown, 'u'}
[11702]  {:unknown, ' '}
[11703]  {:unknown, 'w'}
[11704]  {:unknown, 'i'}
[11705]  {:unknown, 't'}
[11706]  {:unknown, 'h'}
[11707]  {:unknown, ' '}
[11708]  {:unknown, 'y'}
[11709]  {:unknown, 'o'}
[11710]  {:unknown, 'u'}
[11711]  {:unknown, 'r'}
[11712]  {:unknown, ' '}
[11713]  {:unknown, 'q'}
[11714]  {:unknown, 'u'}
[11715]  {:unknown, 'e'}
[11716]  {:unknown, 's'}
[11717]  {:unknown, 't'}
[11718]  {:unknown, '.'}
[11719]  {:jmp, {:value, 105}}
[11721]  {:unknown, 'n'}
[11722]  {:unknown, 's'}
[11723]  {:unknown, 'i'}
[11724]  {:unknown, 'd'}
[11725]  {:unknown, 'e'}
[11726]  {:gt, {:value, 66}, {:value, 101}, {:value, 97}}
[11730]  {:unknown, 'c'}
[11731]  {:unknown, 'h'}
[11732]  {:unknown, [170]}
[11733]  {:unknown, 'T'}
[11734]  {:unknown, 'h'}
[11735]  {:unknown, 'i'}
[11736]  {:unknown, 's'}
[11737]  {:unknown, ' '}
[11738]  {:unknown, 'i'}
[11739]  {:unknown, 's'}
[11740]  {:unknown, ' '}
[11741]  {:unknown, 'a'}
[11742]  {:unknown, ' '}
[11743]  {:unknown, 's'}
[11744]  {:unknown, 'a'}
[11745]  {:unknown, 'n'}
[11746]  {:unknown, 'd'}
[11747]  {:unknown, 'y'}
[11748]  {:unknown, ' '}
[11749]  {:unknown, 'b'}
[11750]  {:unknown, 'e'}
[11751]  {:unknown, 'a'}
[11752]  {:unknown, 'c'}
[11753]  {:unknown, 'h'}
[11754]  {:unknown, ' '}
[11755]  {:unknown, 'i'}
[11756]  {:unknown, 'n'}
[11757]  {:unknown, ' '}
[11758]  {:unknown, 'a'}
[11759]  {:unknown, ' '}
[11760]  {:unknown, 'c'}
[11761]  {:unknown, 'o'}
[11762]  {:unknown, 'v'}
[11763]  {:unknown, 'e'}
[11764]  {:unknown, ' '}
[11765]  {:unknown, 'o'}
[11766]  {:unknown, 'n'}
[11767]  {:unknown, ' '}
[11768]  {:unknown, 's'}
[11769]  {:unknown, 'o'}
[11770]  {:unknown, 'm'}
[11771]  {:unknown, 'e'}
[11772]  {:unknown, ' '}
[11773]  {:unknown, 't'}
[11774]  {:unknown, 'r'}
[11775]  {:unknown, 'o'}
[11776]  {:unknown, 'p'}
[11777]  {:unknown, 'i'}
[11778]  {:unknown, 'c'}
[11779]  {:unknown, 'a'}
[11780]  {:unknown, 'l'}
[11781]  {:unknown, ' '}
[11782]  {:unknown, 'i'}
[11783]  {:unknown, 's'}
[11784]  {:unknown, 'l'}
[11785]  {:unknown, 'a'}
[11786]  {:unknown, 'n'}
[11787]  {:unknown, 'd'}
[11788]  {:unknown, '.'}
[11789]  {:unknown, ' '}
[11790]  {:unknown, ' '}
[11791]  {:unknown, 'I'}
[11792]  {:unknown, 't'}
[11793]  {:unknown, ' '}
[11794]  {:unknown, 'i'}
[11795]  {:unknown, 's'}
[11796]  {:unknown, ' '}
[11797]  {:unknown, 'r'}
[11798]  {:unknown, 'a'}
[11799]  {:unknown, 'i'}
[11800]  {:unknown, 'n'}
[11801]  {:unknown, 'i'}
[11802]  {:unknown, 'n'}
[11803]  {:unknown, 'g'}
[11804]  {:unknown, '.'}
[11805]  {:unknown, ' '}
[11806]  {:unknown, ' '}
[11807]  {:unknown, 'T'}
[11808]  {:unknown, 'h'}
[11809]  {:unknown, 'e'}
[11810]  {:unknown, ' '}
[11811]  {:unknown, 'o'}
[11812]  {:unknown, 'c'}
[11813]  {:unknown, 'e'}
[11814]  {:unknown, 'a'}
[11815]  {:unknown, 'n'}
[11816]  {:unknown, ' '}
[11817]  {:unknown, 'i'}
[11818]  {:unknown, 's'}
[11819]  {:unknown, ' '}
[11820]  {:unknown, 't'}
[11821]  {:unknown, 'o'}
[11822]  {:unknown, ' '}
[11823]  {:unknown, 'y'}
[11824]  {:unknown, 'o'}
[11825]  {:unknown, 'u'}
[11826]  {:unknown, 'r'}
[11827]  {:unknown, ' '}
[11828]  {:unknown, 's'}
[11829]  {:unknown, 'o'}
[11830]  {:unknown, 'u'}
[11831]  {:unknown, 't'}
[11832]  {:unknown, 'h'}
[11833]  {:unknown, ','}
[11834]  {:unknown, ' '}
[11835]  {:unknown, 'a'}
[11836]  {:unknown, 'n'}
[11837]  {:unknown, 'd'}
[11838]  {:unknown, ' '}
[11839]  {:unknown, 'h'}
[11840]  {:unknown, 'e'}
[11841]  {:unknown, 'a'}
[11842]  {:unknown, 'v'}
[11843]  {:unknown, 'y'}
[11844]  {:unknown, ' '}
[11845]  {:unknown, 'f'}
[11846]  {:unknown, 'o'}
[11847]  {:unknown, 'l'}
[11848]  {:unknown, 'i'}
[11849]  {:unknown, 'a'}
[11850]  {:unknown, 'g'}
[11851]  {:unknown, 'e'}
[11852]  {:unknown, ' '}
[11853]  {:unknown, 'i'}
[11854]  {:unknown, 's'}
[11855]  {:unknown, ' '}
[11856]  {:unknown, 't'}
[11857]  {:unknown, 'o'}
[11858]  {:unknown, ' '}
[11859]  {:unknown, 'y'}
[11860]  {:unknown, 'o'}
[11861]  {:unknown, 'u'}
[11862]  {:unknown, 'r'}
[11863]  {:unknown, ' '}
[11864]  {:unknown, 'n'}
[11865]  {:unknown, 'o'}
[11866]  {:unknown, 'r'}
[11867]  {:unknown, 't'}
[11868]  {:unknown, 'h'}
[11869]  {:unknown, ';'}
[11870]  {:unknown, ' '}
[11871]  {:unknown, 't'}
[11872]  {:unknown, 'h'}
[11873]  {:unknown, 'e'}
[11874]  {:unknown, ' '}
[11875]  {:unknown, 'b'}
[11876]  {:unknown, 'e'}
[11877]  {:unknown, 'a'}
[11878]  {:unknown, 'c'}
[11879]  {:unknown, 'h'}
[11880]  {:unknown, ' '}
[11881]  {:unknown, 'e'}
[11882]  {:unknown, 'x'}
[11883]  {:unknown, 't'}
[11884]  {:unknown, 'e'}
[11885]  {:unknown, 'n'}
[11886]  {:unknown, 'd'}
[11887]  {:unknown, 's'}
[11888]  {:unknown, ' '}
[11889]  {:unknown, 'w'}
[11890]  {:unknown, 'e'}
[11891]  {:unknown, 's'}
[11892]  {:unknown, 't'}
[11893]  {:unknown, ' '}
[11894]  {:unknown, 'a'}
[11895]  {:unknown, 'n'}
[11896]  {:unknown, 'd'}
[11897]  {:unknown, ' '}
[11898]  {:unknown, 'e'}
[11899]  {:unknown, 'a'}
[11900]  {:unknown, 's'}
[11901]  {:unknown, 't'}
[11902]  {:unknown, '.'}
[11903]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[11907]  {:unknown, 't'}
[11908]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[11912]  {:unknown, 't'}
[11913]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[11917]  {:unknown, 't'}
[11918]  {:unknown, 'h'}
[11919]  {:gt, {:value, 66}, {:value, 101}, {:value, 97}}
[11923]  {:unknown, 'c'}
[11924]  {:unknown, 'h'}
[11925]  {:unknown, [262]}
[11926]  {:unknown, 'T'}
[11927]  {:unknown, 'h'}
[11928]  {:unknown, 'i'}
[11929]  {:unknown, 's'}
[11930]  {:unknown, ' '}
[11931]  {:unknown, 'i'}
[11932]  {:unknown, 's'}
[11933]  {:unknown, ' '}
[11934]  {:unknown, 'a'}
[11935]  {:unknown, ' '}
[11936]  {:unknown, 's'}
[11937]  {:unknown, 'a'}
[11938]  {:unknown, 'n'}
[11939]  {:unknown, 'd'}
[11940]  {:unknown, 'y'}
[11941]  {:unknown, ' '}
[11942]  {:unknown, 'b'}
[11943]  {:unknown, 'e'}
[11944]  {:unknown, 'a'}
[11945]  {:unknown, 'c'}
[11946]  {:unknown, 'h'}
[11947]  {:unknown, ' '}
[11948]  {:unknown, 'i'}
[11949]  {:unknown, 'n'}
[11950]  {:unknown, ' '}
[11951]  {:unknown, 'a'}
[11952]  {:unknown, ' '}
[11953]  {:unknown, 'c'}
[11954]  {:unknown, 'o'}
[11955]  {:unknown, 'v'}
[11956]  {:unknown, 'e'}
[11957]  {:unknown, ' '}
[11958]  {:unknown, 'o'}
[11959]  {:unknown, 'n'}
[11960]  {:unknown, ' '}
[11961]  {:unknown, 's'}
[11962]  {:unknown, 'o'}
[11963]  {:unknown, 'm'}
[11964]  {:unknown, 'e'}
[11965]  {:unknown, ' '}
[11966]  {:unknown, 't'}
[11967]  {:unknown, 'r'}
[11968]  {:unknown, 'o'}
[11969]  {:unknown, 'p'}
[11970]  {:unknown, 'i'}
[11971]  {:unknown, 'c'}
[11972]  {:unknown, 'a'}
[11973]  {:unknown, 'l'}
[11974]  {:unknown, ' '}
[11975]  {:unknown, 'i'}
[11976]  {:unknown, 's'}
[11977]  {:unknown, 'l'}
[11978]  {:unknown, 'a'}
[11979]  {:unknown, 'n'}
[11980]  {:unknown, 'd'}
[11981]  {:unknown, '.'}
[11982]  {:unknown, ' '}
[11983]  {:unknown, ' '}
[11984]  {:unknown, 'I'}
[11985]  {:unknown, 't'}
[11986]  {:unknown, ' '}
[11987]  {:unknown, 'i'}
[11988]  {:unknown, 's'}
[11989]  {:unknown, ' '}
[11990]  {:unknown, 'r'}
[11991]  {:unknown, 'a'}
[11992]  {:unknown, 'i'}
[11993]  {:unknown, 'n'}
[11994]  {:unknown, 'i'}
[11995]  {:unknown, 'n'}
[11996]  {:unknown, 'g'}
[11997]  {:unknown, '.'}
[11998]  {:unknown, ' '}
[11999]  {:unknown, ' '}
[12000]  {:unknown, 'T'}
[12001]  {:unknown, 'o'}
[12002]  {:unknown, ' '}
[12003]  {:unknown, 'y'}
[12004]  {:unknown, 'o'}
[12005]  {:unknown, 'u'}
[12006]  {:unknown, 'r'}
[12007]  {:unknown, ' '}
[12008]  {:unknown, 'w'}
[12009]  {:unknown, 'e'}
[12010]  {:unknown, 's'}
[12011]  {:unknown, 't'}
[12012]  {:unknown, ' '}
[12013]  {:unknown, 'i'}
[12014]  {:unknown, 's'}
[12015]  {:unknown, ' '}
[12016]  {:unknown, 'a'}
[12017]  {:unknown, 'n'}
[12018]  {:unknown, ' '}
[12019]  {:unknown, 'e'}
[12020]  {:unknown, 'm'}
[12021]  {:unknown, 'b'}
[12022]  {:unknown, 'a'}
[12023]  {:unknown, 'n'}
[12024]  {:unknown, 'k'}
[12025]  {:unknown, 'm'}
[12026]  {:unknown, 'e'}
[12027]  {:unknown, 'n'}
[12028]  {:unknown, 't'}
[12029]  {:unknown, ' '}
[12030]  {:unknown, 'o'}
[12031]  {:unknown, 'f'}
[12032]  {:unknown, ' '}
[12033]  {:unknown, 't'}
[12034]  {:unknown, 'h'}
[12035]  {:unknown, 'e'}
[12036]  {:unknown, ' '}
[12037]  {:unknown, 'c'}
[12038]  {:unknown, 'o'}
[12039]  {:unknown, 'v'}
[12040]  {:unknown, 'e'}
[12041]  {:unknown, ' '}
[12042]  {:unknown, 'w'}
[12043]  {:unknown, 'h'}
[12044]  {:unknown, 'i'}
[12045]  {:unknown, 'c'}
[12046]  {:unknown, 'h'}
[12047]  {:unknown, ' '}
[12048]  {:unknown, 'l'}
[12049]  {:unknown, 'o'}
[12050]  {:unknown, 'o'}
[12051]  {:unknown, 'k'}
[12052]  {:unknown, 's'}
[12053]  {:unknown, ' '}
[12054]  {:unknown, 't'}
[12055]  {:unknown, 'o'}
[12056]  {:unknown, 'o'}
[12057]  {:unknown, ' '}
[12058]  {:unknown, 's'}
[12059]  {:unknown, 't'}
[12060]  {:unknown, 'e'}
[12061]  {:unknown, 'e'}
[12062]  {:unknown, 'p'}
[12063]  {:unknown, ' '}
[12064]  {:unknown, 't'}
[12065]  {:unknown, 'o'}
[12066]  {:unknown, ' '}
[12067]  {:unknown, 'c'}
[12068]  {:unknown, 'l'}
[12069]  {:unknown, 'i'}
[12070]  {:unknown, 'm'}
[12071]  {:unknown, 'b'}
[12072]  {:unknown, '.'}
[12073]  {:unknown, ' '}
[12074]  {:unknown, ' '}
[12075]  {:unknown, 'T'}
[12076]  {:unknown, 'h'}
[12077]  {:unknown, 'e'}
[12078]  {:unknown, ' '}
[12079]  {:unknown, 'b'}
[12080]  {:unknown, 'e'}
[12081]  {:unknown, 'a'}
[12082]  {:unknown, 'c'}
[12083]  {:unknown, 'h'}
[12084]  {:unknown, ' '}
[12085]  {:unknown, 'e'}
[12086]  {:unknown, 'x'}
[12087]  {:unknown, 't'}
[12088]  {:unknown, 'e'}
[12089]  {:unknown, 'n'}
[12090]  {:unknown, 'd'}
[12091]  {:unknown, 's'}
[12092]  {:unknown, ' '}
[12093]  {:unknown, 't'}
[12094]  {:unknown, 'o'}
[12095]  {:unknown, ' '}
[12096]  {:unknown, 't'}
[12097]  {:unknown, 'h'}
[12098]  {:unknown, 'e'}
[12099]  {:unknown, ' '}
[12100]  {:unknown, 'e'}
[12101]  {:unknown, 'a'}
[12102]  {:unknown, 's'}
[12103]  {:unknown, 't'}
[12104]  {:unknown, ','}
[12105]  {:unknown, ' '}
[12106]  {:unknown, 'a'}
[12107]  {:unknown, 'n'}
[12108]  {:unknown, 'd'}
[12109]  {:unknown, ' '}
[12110]  {:unknown, 't'}
[12111]  {:unknown, 'h'}
[12112]  {:unknown, 'e'}
[12113]  {:unknown, 'r'}
[12114]  {:unknown, 'e'}
[12115]  {:unknown, ' '}
[12116]  {:unknown, 'i'}
[12117]  {:unknown, 's'}
[12118]  {:unknown, ' '}
[12119]  {:unknown, 'd'}
[12120]  {:unknown, 'e'}
[12121]  {:unknown, 'n'}
[12122]  {:unknown, 's'}
[12123]  {:unknown, 'e'}
[12124]  {:unknown, ' '}
[12125]  {:unknown, 'f'}
[12126]  {:unknown, 'o'}
[12127]  {:unknown, 'l'}
[12128]  {:unknown, 'i'}
[12129]  {:unknown, 'a'}
[12130]  {:unknown, 'g'}
[12131]  {:unknown, 'e'}
[12132]  {:unknown, ' '}
[12133]  {:unknown, 't'}
[12134]  {:unknown, 'o'}
[12135]  {:unknown, ' '}
[12136]  {:unknown, 't'}
[12137]  {:unknown, 'h'}
[12138]  {:unknown, 'e'}
[12139]  {:unknown, ' '}
[12140]  {:unknown, 'n'}
[12141]  {:unknown, 'o'}
[12142]  {:unknown, 'r'}
[12143]  {:unknown, 't'}
[12144]  {:unknown, 'h'}
[12145]  {:unknown, '.'}
[12146]  {:unknown, ' '}
[12147]  {:unknown, ' '}
[12148]  {:unknown, 'T'}
[12149]  {:unknown, 'h'}
[12150]  {:unknown, 'e'}
[12151]  {:unknown, ' '}
[12152]  {:unknown, 'o'}
[12153]  {:unknown, 'c'}
[12154]  {:unknown, 'e'}
[12155]  {:unknown, 'a'}
[12156]  {:unknown, 'n'}
[12157]  {:unknown, ' '}
[12158]  {:unknown, 't'}
[12159]  {:unknown, 'o'}
[12160]  {:unknown, ' '}
[12161]  {:unknown, 't'}
[12162]  {:unknown, 'h'}
[12163]  {:unknown, 'e'}
[12164]  {:unknown, ' '}
[12165]  {:unknown, 's'}
[12166]  {:unknown, 'o'}
[12167]  {:unknown, 'u'}
[12168]  {:unknown, 't'}
[12169]  {:unknown, 'h'}
[12170]  {:unknown, ' '}
[12171]  {:unknown, 's'}
[12172]  {:unknown, 'e'}
[12173]  {:unknown, 'e'}
[12174]  {:unknown, 'm'}
[12175]  {:unknown, 's'}
[12176]  {:unknown, ' '}
[12177]  {:unknown, 'u'}
[12178]  {:unknown, 'n'}
[12179]  {:unknown, 'i'}
[12180]  {:unknown, 'n'}
[12181]  {:unknown, 'v'}
[12182]  {:unknown, 'i'}
[12183]  {:unknown, 't'}
[12184]  {:unknown, 'i'}
[12185]  {:unknown, 'n'}
[12186]  {:unknown, 'g'}
[12187]  {:unknown, '.'}
[12188]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[12192]  {:unknown, 't'}
[12193]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[12197]  {:unknown, 't'}
[12198]  {:unknown, 'h'}
[12199]  {:gt, {:value, 66}, {:value, 101}, {:value, 97}}
[12203]  {:unknown, 'c'}
[12204]  {:unknown, 'h'}
[12205]  {:unknown, [242]}
[12206]  {:unknown, 'T'}
[12207]  {:unknown, 'h'}
[12208]  {:unknown, 'i'}
[12209]  {:unknown, 's'}
[12210]  {:unknown, ' '}
[12211]  {:unknown, 'i'}
[12212]  {:unknown, 's'}
[12213]  {:unknown, ' '}
[12214]  {:unknown, 'a'}
[12215]  {:unknown, ' '}
[12216]  {:unknown, 's'}
[12217]  {:unknown, 'a'}
[12218]  {:unknown, 'n'}
[12219]  {:unknown, 'd'}
[12220]  {:unknown, 'y'}
[12221]  {:unknown, ' '}
[12222]  {:unknown, 'b'}
[12223]  {:unknown, 'e'}
[12224]  {:unknown, 'a'}
[12225]  {:unknown, 'c'}
[12226]  {:unknown, 'h'}
[12227]  {:unknown, ' '}
[12228]  {:unknown, 'i'}
[12229]  {:unknown, 'n'}
[12230]  {:unknown, ' '}
[12231]  {:unknown, 'a'}
[12232]  {:unknown, ' '}
[12233]  {:unknown, 'c'}
[12234]  {:unknown, 'o'}
[12235]  {:unknown, 'v'}
[12236]  {:unknown, 'e'}
[12237]  {:unknown, ' '}
[12238]  {:unknown, 'o'}
[12239]  {:unknown, 'n'}
[12240]  {:unknown, ' '}
[12241]  {:unknown, 's'}
[12242]  {:unknown, 'o'}
[12243]  {:unknown, 'm'}
[12244]  {:unknown, 'e'}
[12245]  {:unknown, ' '}
[12246]  {:unknown, 't'}
[12247]  {:unknown, 'r'}
[12248]  {:unknown, 'o'}
[12249]  {:unknown, 'p'}
[12250]  {:unknown, 'i'}
[12251]  {:unknown, 'c'}
[12252]  {:unknown, 'a'}
[12253]  {:unknown, 'l'}
[12254]  {:unknown, ' '}
[12255]  {:unknown, 'i'}
[12256]  {:unknown, 's'}
[12257]  {:unknown, 'l'}
[12258]  {:unknown, 'a'}
[12259]  {:unknown, 'n'}
[12260]  {:unknown, 'd'}
[12261]  {:unknown, '.'}
[12262]  {:unknown, ' '}
[12263]  {:unknown, ' '}
[12264]  {:unknown, 'I'}
[12265]  {:unknown, 't'}
[12266]  {:unknown, ' '}
[12267]  {:unknown, 'i'}
[12268]  {:unknown, 's'}
[12269]  {:unknown, ' '}
[12270]  {:unknown, 'r'}
[12271]  {:unknown, 'a'}
[12272]  {:unknown, 'i'}
[12273]  {:unknown, 'n'}
[12274]  {:unknown, 'i'}
[12275]  {:unknown, 'n'}
[12276]  {:unknown, 'g'}
[12277]  {:unknown, '.'}
[12278]  {:unknown, ' '}
[12279]  {:unknown, ' '}
[12280]  {:unknown, 'T'}
[12281]  {:unknown, 'h'}
[12282]  {:unknown, 'e'}
[12283]  {:unknown, ' '}
[12284]  {:unknown, 's'}
[12285]  {:unknown, 't'}
[12286]  {:unknown, 'e'}
[12287]  {:unknown, 'e'}
[12288]  {:unknown, 'p'}
[12289]  {:unknown, ' '}
[12290]  {:unknown, 'c'}
[12291]  {:unknown, 'o'}
[12292]  {:unknown, 'v'}
[12293]  {:unknown, 'e'}
[12294]  {:unknown, ' '}
[12295]  {:unknown, 'e'}
[12296]  {:unknown, 'm'}
[12297]  {:unknown, 'b'}
[12298]  {:unknown, 'a'}
[12299]  {:unknown, 'n'}
[12300]  {:unknown, 'k'}
[12301]  {:unknown, 'm'}
[12302]  {:unknown, 'e'}
[12303]  {:unknown, 'n'}
[12304]  {:unknown, 't'}
[12305]  {:unknown, ' '}
[12306]  {:unknown, 't'}
[12307]  {:unknown, 'o'}
[12308]  {:unknown, ' '}
[12309]  {:unknown, 'y'}
[12310]  {:unknown, 'o'}
[12311]  {:unknown, 'u'}
[12312]  {:unknown, 'r'}
[12313]  {:unknown, ' '}
[12314]  {:unknown, 'e'}
[12315]  {:unknown, 'a'}
[12316]  {:unknown, 's'}
[12317]  {:unknown, 't'}
[12318]  {:unknown, ' '}
[12319]  {:unknown, 'b'}
[12320]  {:unknown, 'l'}
[12321]  {:unknown, 'o'}
[12322]  {:unknown, 'c'}
[12323]  {:unknown, 'k'}
[12324]  {:unknown, 's'}
[12325]  {:unknown, ' '}
[12326]  {:unknown, 'y'}
[12327]  {:unknown, 'o'}
[12328]  {:unknown, 'u'}
[12329]  {:unknown, 'r'}
[12330]  {:unknown, ' '}
[12331]  {:unknown, 'p'}
[12332]  {:unknown, 'a'}
[12333]  {:unknown, 't'}
[12334]  {:unknown, 'h'}
[12335]  {:unknown, ','}
[12336]  {:unknown, ' '}
[12337]  {:unknown, 'a'}
[12338]  {:unknown, 'n'}
[12339]  {:unknown, 'd'}
[12340]  {:unknown, ' '}
[12341]  {:unknown, 't'}
[12342]  {:unknown, 'h'}
[12343]  {:unknown, 'e'}
[12344]  {:unknown, ' '}
[12345]  {:unknown, 'o'}
[12346]  {:unknown, 'c'}
[12347]  {:unknown, 'e'}
[12348]  {:unknown, 'a'}
[12349]  {:unknown, 'n'}
[12350]  {:unknown, ' '}
[12351]  {:unknown, 'w'}
[12352]  {:unknown, 'a'}
[12353]  {:unknown, 't'}
[12354]  {:unknown, 'e'}
[12355]  {:unknown, 'r'}
[12356]  {:unknown, 's'}
[12357]  {:unknown, ' '}
[12358]  {:unknown, 'h'}
[12359]  {:unknown, 'e'}
[12360]  {:unknown, 'r'}
[12361]  {:unknown, 'e'}
[12362]  {:unknown, ' '}
[12363]  {:unknown, 'l'}
[12364]  {:unknown, 'o'}
[12365]  {:unknown, 'o'}
[12366]  {:unknown, 'k'}
[12367]  {:unknown, ' '}
[12368]  {:unknown, 'u'}
[12369]  {:unknown, 'n'}
[12370]  {:unknown, 's'}
[12371]  {:unknown, 'a'}
[12372]  {:unknown, 'f'}
[12373]  {:unknown, 'e'}
[12374]  {:unknown, '.'}
[12375]  {:unknown, ' '}
[12376]  {:unknown, ' '}
[12377]  {:unknown, 'T'}
[12378]  {:unknown, 'h'}
[12379]  {:unknown, 'e'}
[12380]  {:unknown, ' '}
[12381]  {:unknown, 'b'}
[12382]  {:unknown, 'e'}
[12383]  {:unknown, 'a'}
[12384]  {:unknown, 'c'}
[12385]  {:unknown, 'h'}
[12386]  {:unknown, ' '}
[12387]  {:unknown, 'e'}
[12388]  {:unknown, 'x'}
[12389]  {:unknown, 't'}
[12390]  {:unknown, 'e'}
[12391]  {:unknown, 'n'}
[12392]  {:unknown, 'd'}
[12393]  {:unknown, 's'}
[12394]  {:unknown, ' '}
[12395]  {:unknown, 't'}
[12396]  {:unknown, 'o'}
[12397]  {:unknown, ' '}
[12398]  {:unknown, 't'}
[12399]  {:unknown, 'h'}
[12400]  {:unknown, 'e'}
[12401]  {:unknown, ' '}
[12402]  {:unknown, 'w'}
[12403]  {:unknown, 'e'}
[12404]  {:unknown, 's'}
[12405]  {:unknown, 't'}
[12406]  {:unknown, ','}
[12407]  {:unknown, ' '}
[12408]  {:unknown, 'a'}
[12409]  {:unknown, 'n'}
[12410]  {:unknown, 'd'}
[12411]  {:unknown, ' '}
[12412]  {:unknown, 't'}
[12413]  {:unknown, 'h'}
[12414]  {:unknown, 'e'}
[12415]  {:unknown, 'r'}
[12416]  {:unknown, 'e'}
[12417]  {:unknown, ' '}
[12418]  {:unknown, 'i'}
[12419]  {:unknown, 's'}
[12420]  {:unknown, ' '}
[12421]  {:unknown, 'd'}
[12422]  {:unknown, 'e'}
[12423]  {:unknown, 'n'}
[12424]  {:unknown, 's'}
[12425]  {:unknown, 'e'}
[12426]  {:unknown, ' '}
[12427]  {:unknown, 'f'}
[12428]  {:unknown, 'o'}
[12429]  {:unknown, 'l'}
[12430]  {:unknown, 'i'}
[12431]  {:unknown, 'a'}
[12432]  {:unknown, 'g'}
[12433]  {:unknown, 'e'}
[12434]  {:unknown, ' '}
[12435]  {:unknown, 't'}
[12436]  {:unknown, 'o'}
[12437]  {:unknown, ' '}
[12438]  {:unknown, 't'}
[12439]  {:unknown, 'h'}
[12440]  {:unknown, 'e'}
[12441]  {:unknown, ' '}
[12442]  {:unknown, 'n'}
[12443]  {:unknown, 'o'}
[12444]  {:unknown, 'r'}
[12445]  {:unknown, 't'}
[12446]  {:unknown, 'h'}
[12447]  {:unknown, '.'}
[12448]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[12452]  {:unknown, 't'}
[12453]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[12457]  {:unknown, 't'}
[12458]  {:unknown, 'h'}
[12459]  {:rmem, {:value, 84}, {:value, 114}}
[12462]  {:unknown, 'o'}
[12463]  {:unknown, 'p'}
[12464]  {:unknown, 'i'}
[12465]  {:unknown, 'c'}
[12466]  {:unknown, 'a'}
[12467]  {:unknown, 'l'}
[12468]  {:unknown, ' '}
[12469]  {:unknown, 'I'}
[12470]  {:unknown, 's'}
[12471]  {:unknown, 'l'}
[12472]  {:unknown, 'a'}
[12473]  {:unknown, 'n'}
[12474]  {:unknown, 'd'}
[12475]  {:unknown, [198]}
[12476]  {:unknown, 'T'}
[12477]  {:unknown, 'h'}
[12478]  {:unknown, 'e'}
[12479]  {:unknown, ' '}
[12480]  {:unknown, 'l'}
[12481]  {:unknown, 'a'}
[12482]  {:unknown, 'r'}
[12483]  {:unknown, 'g'}
[12484]  {:unknown, 'e'}
[12485]  {:unknown, ' '}
[12486]  {:unknown, 't'}
[12487]  {:unknown, 'r'}
[12488]  {:unknown, 'e'}
[12489]  {:unknown, 'e'}
[12490]  {:unknown, 's'}
[12491]  {:unknown, ' '}
[12492]  {:unknown, 'h'}
[12493]  {:unknown, 'e'}
[12494]  {:unknown, 'r'}
[12495]  {:unknown, 'e'}
[12496]  {:unknown, ' '}
[12497]  {:unknown, 's'}
[12498]  {:unknown, 'e'}
[12499]  {:unknown, 'e'}
[12500]  {:unknown, 'm'}
[12501]  {:unknown, ' '}
[12502]  {:unknown, 't'}
[12503]  {:unknown, 'o'}
[12504]  {:unknown, ' '}
[12505]  {:unknown, 'b'}
[12506]  {:unknown, 'e'}
[12507]  {:unknown, ' '}
[12508]  {:unknown, 'p'}
[12509]  {:unknown, 'r'}
[12510]  {:unknown, 'o'}
[12511]  {:unknown, 't'}
[12512]  {:unknown, 'e'}
[12513]  {:unknown, 'c'}
[12514]  {:unknown, 't'}
[12515]  {:unknown, 'i'}
[12516]  {:unknown, 'n'}
[12517]  {:unknown, 'g'}
[12518]  {:unknown, ' '}
[12519]  {:unknown, 'y'}
[12520]  {:unknown, 'o'}
[12521]  {:unknown, 'u'}
[12522]  {:unknown, ' '}
[12523]  {:unknown, 'f'}
[12524]  {:unknown, 'r'}
[12525]  {:unknown, 'o'}
[12526]  {:unknown, 'm'}
[12527]  {:unknown, ' '}
[12528]  {:unknown, 't'}
[12529]  {:unknown, 'h'}
[12530]  {:unknown, 'e'}
[12531]  {:unknown, ' '}
[12532]  {:unknown, 'r'}
[12533]  {:unknown, 'a'}
[12534]  {:unknown, 'i'}
[12535]  {:unknown, 'n'}
[12536]  {:unknown, '.'}
[12537]  {:unknown, ' '}
[12538]  {:unknown, ' '}
[12539]  {:unknown, 'A'}
[12540]  {:unknown, 's'}
[12541]  {:unknown, ' '}
[12542]  {:unknown, 'y'}
[12543]  {:unknown, 'o'}
[12544]  {:unknown, 'u'}
[12545]  {:unknown, ' '}
[12546]  {:unknown, 'p'}
[12547]  {:unknown, 'u'}
[12548]  {:unknown, 's'}
[12549]  {:unknown, 'h'}
[12550]  {:unknown, ' '}
[12551]  {:unknown, 't'}
[12552]  {:unknown, 'h'}
[12553]  {:unknown, 'r'}
[12554]  {:unknown, 'o'}
[12555]  {:unknown, 'u'}
[12556]  {:unknown, 'g'}
[12557]  {:unknown, 'h'}
[12558]  {:unknown, ' '}
[12559]  {:unknown, 't'}
[12560]  {:unknown, 'h'}
[12561]  {:unknown, 'e'}
[12562]  {:unknown, ' '}
[12563]  {:unknown, 'u'}
[12564]  {:unknown, 'n'}
[12565]  {:unknown, 'd'}
[12566]  {:unknown, 'e'}
[12567]  {:unknown, 'r'}
[12568]  {:unknown, 'g'}
[12569]  {:unknown, 'r'}
[12570]  {:unknown, 'o'}
[12571]  {:unknown, 'w'}
[12572]  {:unknown, 't'}
[12573]  {:unknown, 'h'}
[12574]  {:unknown, ','}
[12575]  {:unknown, ' '}
[12576]  {:unknown, 'y'}
[12577]  {:unknown, 'o'}
[12578]  {:unknown, 'u'}
[12579]  {:unknown, ' '}
[12580]  {:unknown, 'c'}
[12581]  {:unknown, 'a'}
[12582]  {:unknown, 'n'}
[12583]  {:unknown, ' '}
[12584]  {:unknown, 'h'}
[12585]  {:unknown, 'e'}
[12586]  {:unknown, 'a'}
[12587]  {:unknown, 'r'}
[12588]  {:unknown, ' '}
[12589]  {:unknown, 'b'}
[12590]  {:unknown, 'i'}
[12591]  {:unknown, 'r'}
[12592]  {:unknown, 'd'}
[12593]  {:unknown, 's'}
[12594]  {:unknown, ' '}
[12595]  {:unknown, 'c'}
[12596]  {:unknown, 'h'}
[12597]  {:unknown, 'i'}
[12598]  {:unknown, 'r'}
[12599]  {:unknown, 'p'}
[12600]  {:unknown, 'i'}
[12601]  {:unknown, 'n'}
[12602]  {:unknown, 'g'}
[12603]  {:unknown, ' '}
[12604]  {:unknown, 'o'}
[12605]  {:unknown, 'v'}
[12606]  {:unknown, 'e'}
[12607]  {:unknown, 'r'}
[12608]  {:unknown, 'h'}
[12609]  {:unknown, 'e'}
[12610]  {:unknown, 'a'}
[12611]  {:unknown, 'd'}
[12612]  {:unknown, '.'}
[12613]  {:unknown, ' '}
[12614]  {:unknown, ' '}
[12615]  {:unknown, 'T'}
[12616]  {:unknown, 'h'}
[12617]  {:unknown, 'e'}
[12618]  {:unknown, 'r'}
[12619]  {:unknown, 'e'}
[12620]  {:unknown, ' '}
[12621]  {:unknown, 'i'}
[12622]  {:unknown, 's'}
[12623]  {:unknown, ' '}
[12624]  {:unknown, 'a'}
[12625]  {:unknown, ' '}
[12626]  {:unknown, 's'}
[12627]  {:unknown, 't'}
[12628]  {:unknown, 'e'}
[12629]  {:unknown, 'e'}
[12630]  {:unknown, 'p'}
[12631]  {:unknown, ' '}
[12632]  {:unknown, 'r'}
[12633]  {:unknown, 'o'}
[12634]  {:unknown, 'c'}
[12635]  {:unknown, 'k'}
[12636]  {:unknown, ' '}
[12637]  {:unknown, 'f'}
[12638]  {:unknown, 'a'}
[12639]  {:unknown, 'c'}
[12640]  {:unknown, 'e'}
[12641]  {:unknown, ' '}
[12642]  {:unknown, 't'}
[12643]  {:unknown, 'o'}
[12644]  {:unknown, ' '}
[12645]  {:unknown, 'y'}
[12646]  {:unknown, 'o'}
[12647]  {:unknown, 'u'}
[12648]  {:unknown, 'r'}
[12649]  {:unknown, ' '}
[12650]  {:unknown, 'w'}
[12651]  {:unknown, 'e'}
[12652]  {:unknown, 's'}
[12653]  {:unknown, 't'}
[12654]  {:unknown, ' '}
[12655]  {:unknown, 'b'}
[12656]  {:unknown, 'l'}
[12657]  {:unknown, 'o'}
[12658]  {:unknown, 'c'}
[12659]  {:unknown, 'k'}
[12660]  {:unknown, 'i'}
[12661]  {:unknown, 'n'}
[12662]  {:unknown, 'g'}
[12663]  {:unknown, ' '}
[12664]  {:unknown, 'y'}
[12665]  {:unknown, 'o'}
[12666]  {:unknown, 'u'}
[12667]  {:unknown, 'r'}
[12668]  {:unknown, ' '}
[12669]  {:unknown, 'p'}
[12670]  {:unknown, 'a'}
[12671]  {:unknown, 't'}
[12672]  {:unknown, 'h'}
[12673]  {:unknown, '.'}
[12674]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[12678]  {:unknown, 't'}
[12679]  {:unknown, 'h'}
[12680]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[12684]  {:unknown, 't'}
[12685]  {:unknown, 'h'}
[12686]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[12690]  {:unknown, 't'}
[12691]  {:rmem, {:value, 84}, {:value, 114}}
[12694]  {:unknown, 'o'}
[12695]  {:unknown, 'p'}
[12696]  {:unknown, 'i'}
[12697]  {:unknown, 'c'}
[12698]  {:unknown, 'a'}
[12699]  {:unknown, 'l'}
[12700]  {:unknown, ' '}
[12701]  {:unknown, 'I'}
[12702]  {:unknown, 's'}
[12703]  {:unknown, 'l'}
[12704]  {:unknown, 'a'}
[12705]  {:unknown, 'n'}
[12706]  {:unknown, 'd'}
[12707]  {:unknown, 'z'}
[12708]  {:unknown, 'T'}
[12709]  {:unknown, 'h'}
[12710]  {:unknown, 'e'}
[12711]  {:unknown, ' '}
[12712]  {:unknown, 'e'}
[12713]  {:unknown, 'a'}
[12714]  {:unknown, 's'}
[12715]  {:unknown, 't'}
[12716]  {:unknown, ' '}
[12717]  {:unknown, 'e'}
[12718]  {:unknown, 'm'}
[12719]  {:unknown, 'b'}
[12720]  {:unknown, 'a'}
[12721]  {:unknown, 'n'}
[12722]  {:unknown, 'k'}
[12723]  {:unknown, 'm'}
[12724]  {:unknown, 'e'}
[12725]  {:unknown, 'n'}
[12726]  {:unknown, 't'}
[12727]  {:unknown, ' '}
[12728]  {:unknown, 'o'}
[12729]  {:unknown, 'f'}
[12730]  {:unknown, ' '}
[12731]  {:unknown, 't'}
[12732]  {:unknown, 'h'}
[12733]  {:unknown, 'e'}
[12734]  {:unknown, ' '}
[12735]  {:unknown, 'c'}
[12736]  {:unknown, 'o'}
[12737]  {:unknown, 'v'}
[12738]  {:unknown, 'e'}
[12739]  {:unknown, ' '}
[12740]  {:unknown, 't'}
[12741]  {:unknown, 'o'}
[12742]  {:unknown, 'w'}
[12743]  {:unknown, 'e'}
[12744]  {:unknown, 'r'}
[12745]  {:unknown, 's'}
[12746]  {:unknown, ' '}
[12747]  {:unknown, 'o'}
[12748]  {:unknown, 'v'}
[12749]  {:unknown, 'e'}
[12750]  {:unknown, 'r'}
[12751]  {:unknown, ' '}
[12752]  {:unknown, 'y'}
[12753]  {:unknown, 'o'}
[12754]  {:unknown, 'u'}
[12755]  {:unknown, '.'}
[12756]  {:unknown, ' '}
[12757]  {:unknown, ' '}
[12758]  {:unknown, 'I'}
[12759]  {:unknown, 't'}
[12760]  {:unknown, ' '}
[12761]  {:unknown, 'p'}
[12762]  {:unknown, 'r'}
[12763]  {:unknown, 'o'}
[12764]  {:unknown, 'd'}
[12765]  {:unknown, 'u'}
[12766]  {:unknown, 'c'}
[12767]  {:unknown, 'e'}
[12768]  {:unknown, 's'}
[12769]  {:unknown, ' '}
[12770]  {:unknown, 'a'}
[12771]  {:unknown, ' '}
[12772]  {:unknown, 's'}
[12773]  {:unknown, 'm'}
[12774]  {:unknown, 'a'}
[12775]  {:unknown, 'l'}
[12776]  {:unknown, 'l'}
[12777]  {:unknown, ' '}
[12778]  {:unknown, 'w'}
[12779]  {:unknown, 'a'}
[12780]  {:unknown, 't'}
[12781]  {:unknown, 'e'}
[12782]  {:unknown, 'r'}
[12783]  {:unknown, 'f'}
[12784]  {:unknown, 'a'}
[12785]  {:unknown, 'l'}
[12786]  {:unknown, 'l'}
[12787]  {:unknown, ' '}
[12788]  {:unknown, 'h'}
[12789]  {:unknown, 'e'}
[12790]  {:unknown, 'r'}
[12791]  {:unknown, 'e'}
[12792]  {:unknown, ' '}
[12793]  {:unknown, 'w'}
[12794]  {:unknown, 'h'}
[12795]  {:unknown, 'i'}
[12796]  {:unknown, 'c'}
[12797]  {:unknown, 'h'}
[12798]  {:unknown, ' '}
[12799]  {:unknown, 'c'}
[12800]  {:unknown, 'a'}
[12801]  {:unknown, 's'}
[12802]  {:unknown, 'c'}
[12803]  {:unknown, 'a'}
[12804]  {:unknown, 'd'}
[12805]  {:unknown, 'e'}
[12806]  {:unknown, 's'}
[12807]  {:unknown, ' '}
[12808]  {:unknown, 'e'}
[12809]  {:unknown, 'x'}
[12810]  {:unknown, 'c'}
[12811]  {:unknown, 'i'}
[12812]  {:unknown, 't'}
[12813]  {:unknown, 'e'}
[12814]  {:unknown, 'd'}
[12815]  {:unknown, 'l'}
[12816]  {:unknown, 'y'}
[12817]  {:unknown, ' '}
[12818]  {:unknown, 'i'}
[12819]  {:unknown, 'n'}
[12820]  {:unknown, 't'}
[12821]  {:unknown, 'o'}
[12822]  {:unknown, ' '}
[12823]  {:unknown, 'a'}
[12824]  {:unknown, ' '}
[12825]  {:unknown, 'p'}
[12826]  {:unknown, 'o'}
[12827]  {:unknown, 'o'}
[12828]  {:unknown, 'l'}
[12829]  {:unknown, '.'}
[12830]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[12834]  {:unknown, 't'}
[12835]  {:unknown, 'h'}
[12836]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[12840]  {:unknown, 't'}
[12841]  {:unknown, 'h'}
[12842]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[12846]  {:unknown, 't'}
[12847]  {:rmem, {:value, 84}, {:value, 114}}
[12850]  {:unknown, 'o'}
[12851]  {:unknown, 'p'}
[12852]  {:unknown, 'i'}
[12853]  {:unknown, 'c'}
[12854]  {:unknown, 'a'}
[12855]  {:unknown, 'l'}
[12856]  {:unknown, ' '}
[12857]  {:unknown, 'I'}
[12858]  {:unknown, 's'}
[12859]  {:unknown, 'l'}
[12860]  {:unknown, 'a'}
[12861]  {:unknown, 'n'}
[12862]  {:unknown, 'd'}
[12863]  {:unknown, [230]}
[12864]  {:unknown, 'T'}
[12865]  {:unknown, 'h'}
[12866]  {:unknown, 'e'}
[12867]  {:unknown, ' '}
[12868]  {:unknown, 'e'}
[12869]  {:unknown, 'm'}
[12870]  {:unknown, 'b'}
[12871]  {:unknown, 'a'}
[12872]  {:unknown, 'n'}
[12873]  {:unknown, 'k'}
[12874]  {:unknown, 'm'}
[12875]  {:unknown, 'e'}
[12876]  {:unknown, 'n'}
[12877]  {:unknown, 't'}
[12878]  {:unknown, ' '}
[12879]  {:unknown, 'o'}
[12880]  {:unknown, 'f'}
[12881]  {:unknown, ' '}
[12882]  {:unknown, 't'}
[12883]  {:unknown, 'h'}
[12884]  {:unknown, 'e'}
[12885]  {:unknown, ' '}
[12886]  {:unknown, 'c'}
[12887]  {:unknown, 'o'}
[12888]  {:unknown, 'v'}
[12889]  {:unknown, 'e'}
[12890]  {:unknown, ' '}
[12891]  {:unknown, 'c'}
[12892]  {:unknown, 'o'}
[12893]  {:unknown, 'm'}
[12894]  {:unknown, 'e'}
[12895]  {:unknown, ' '}
[12896]  {:unknown, 't'}
[12897]  {:unknown, 'o'}
[12898]  {:unknown, 'e'}
[12899]  {:unknown, 'g'}
[12900]  {:unknown, 'h'}
[12901]  {:unknown, 'e'}
[12902]  {:unknown, 'r'}
[12903]  {:unknown, ' '}
[12904]  {:unknown, 'h'}
[12905]  {:unknown, 'e'}
[12906]  {:unknown, 'r'}
[12907]  {:unknown, 'e'}
[12908]  {:unknown, ' '}
[12909]  {:unknown, 't'}
[12910]  {:unknown, 'o'}
[12911]  {:unknown, ' '}
[12912]  {:unknown, 'y'}
[12913]  {:unknown, 'o'}
[12914]  {:unknown, 'u'}
[12915]  {:unknown, 'r'}
[12916]  {:unknown, ' '}
[12917]  {:unknown, 'e'}
[12918]  {:unknown, 'a'}
[12919]  {:unknown, 's'}
[12920]  {:unknown, 't'}
[12921]  {:unknown, ' '}
[12922]  {:unknown, 'a'}
[12923]  {:unknown, 'n'}
[12924]  {:unknown, 'd'}
[12925]  {:unknown, ' '}
[12926]  {:unknown, 'w'}
[12927]  {:unknown, 'e'}
[12928]  {:unknown, 's'}
[12929]  {:unknown, 't'}
[12930]  {:unknown, '.'}
[12931]  {:unknown, ' '}
[12932]  {:unknown, ' '}
[12933]  {:unknown, 'B'}
[12934]  {:unknown, 'e'}
[12935]  {:unknown, 't'}
[12936]  {:unknown, 'w'}
[12937]  {:unknown, 'e'}
[12938]  {:unknown, 'e'}
[12939]  {:unknown, 'n'}
[12940]  {:unknown, ' '}
[12941]  {:unknown, 't'}
[12942]  {:unknown, 'h'}
[12943]  {:unknown, 'e'}
[12944]  {:unknown, 's'}
[12945]  {:unknown, 'e'}
[12946]  {:unknown, ' '}
[12947]  {:unknown, 't'}
[12948]  {:unknown, 'a'}
[12949]  {:unknown, 'l'}
[12950]  {:unknown, 'l'}
[12951]  {:unknown, ' '}
[12952]  {:unknown, 'r'}
[12953]  {:unknown, 'o'}
[12954]  {:unknown, 'c'}
[12955]  {:unknown, 'k'}
[12956]  {:unknown, ' '}
[12957]  {:unknown, 'f'}
[12958]  {:unknown, 'a'}
[12959]  {:unknown, 'c'}
[12960]  {:unknown, 'e'}
[12961]  {:unknown, 's'}
[12962]  {:unknown, ','}
[12963]  {:unknown, ' '}
[12964]  {:unknown, 't'}
[12965]  {:unknown, 'h'}
[12966]  {:unknown, 'e'}
[12967]  {:unknown, 'r'}
[12968]  {:unknown, 'e'}
[12969]  {:unknown, ' '}
[12970]  {:unknown, 'i'}
[12971]  {:unknown, 's'}
[12972]  {:unknown, ' '}
[12973]  {:unknown, 'a'}
[12974]  {:unknown, ' '}
[12975]  {:unknown, 'n'}
[12976]  {:unknown, 'a'}
[12977]  {:unknown, 'r'}
[12978]  {:unknown, 'r'}
[12979]  {:unknown, 'o'}
[12980]  {:unknown, 'w'}
[12981]  {:unknown, ','}
[12982]  {:unknown, ' '}
[12983]  {:unknown, 'o'}
[12984]  {:unknown, 'v'}
[12985]  {:unknown, 'e'}
[12986]  {:unknown, 'r'}
[12987]  {:unknown, 'g'}
[12988]  {:unknown, 'r'}
[12989]  {:unknown, 'o'}
[12990]  {:unknown, 'w'}
[12991]  {:unknown, 'n'}
[12992]  {:unknown, ' '}
[12993]  {:unknown, 'p'}
[12994]  {:unknown, 'a'}
[12995]  {:unknown, 't'}
[12996]  {:unknown, 'h'}
[12997]  {:unknown, ' '}
[12998]  {:unknown, 'l'}
[12999]  {:unknown, 'e'}
[13000]  {:unknown, 'a'}
[13001]  {:unknown, 'd'}
[13002]  {:unknown, 'i'}
[13003]  {:unknown, 'n'}
[13004]  {:unknown, 'g'}
[13005]  {:unknown, ' '}
[13006]  {:unknown, 'n'}
[13007]  {:unknown, 'o'}
[13008]  {:unknown, 'r'}
[13009]  {:unknown, 't'}
[13010]  {:unknown, 'h'}
[13011]  {:unknown, '.'}
[13012]  {:unknown, ' '}
[13013]  {:unknown, ' '}
[13014]  {:unknown, 'Y'}
[13015]  {:unknown, 'o'}
[13016]  {:unknown, 'u'}
[13017]  {:unknown, ' '}
[13018]  {:unknown, 'h'}
[13019]  {:unknown, 'e'}
[13020]  {:unknown, 'a'}
[13021]  {:unknown, 'r'}
[13022]  {:unknown, ' '}
[13023]  {:unknown, 'w'}
[13024]  {:unknown, 'a'}
[13025]  {:unknown, 'v'}
[13026]  {:unknown, 'e'}
[13027]  {:unknown, 's'}
[13028]  {:unknown, ' '}
[13029]  {:unknown, 'l'}
[13030]  {:unknown, 'a'}
[13031]  {:unknown, 'p'}
[13032]  {:unknown, 'p'}
[13033]  {:unknown, 'i'}
[13034]  {:unknown, 'n'}
[13035]  {:unknown, 'g'}
[13036]  {:unknown, ' '}
[13037]  {:unknown, 'u'}
[13038]  {:unknown, 'p'}
[13039]  {:unknown, ' '}
[13040]  {:unknown, 'o'}
[13041]  {:unknown, 'n'}
[13042]  {:unknown, ' '}
[13043]  {:unknown, 'a'}
[13044]  {:unknown, ' '}
[13045]  {:unknown, 'b'}
[13046]  {:unknown, 'e'}
[13047]  {:unknown, 'a'}
[13048]  {:unknown, 'c'}
[13049]  {:unknown, 'h'}
[13050]  {:unknown, ' '}
[13051]  {:unknown, 't'}
[13052]  {:unknown, 'h'}
[13053]  {:unknown, 'r'}
[13054]  {:unknown, 'o'}
[13055]  {:unknown, 'u'}
[13056]  {:unknown, 'g'}
[13057]  {:unknown, 'h'}
[13058]  {:unknown, ' '}
[13059]  {:unknown, 't'}
[13060]  {:unknown, 'h'}
[13061]  {:unknown, 'e'}
[13062]  {:unknown, ' '}
[13063]  {:unknown, 'd'}
[13064]  {:unknown, 'e'}
[13065]  {:unknown, 'n'}
[13066]  {:unknown, 's'}
[13067]  {:unknown, 'e'}
[13068]  {:unknown, ' '}
[13069]  {:unknown, 'v'}
[13070]  {:unknown, 'e'}
[13071]  {:unknown, 'g'}
[13072]  {:unknown, 'e'}
[13073]  {:unknown, 't'}
[13074]  {:unknown, 'a'}
[13075]  {:unknown, 't'}
[13076]  {:unknown, 'i'}
[13077]  {:unknown, 'o'}
[13078]  {:unknown, 'n'}
[13079]  {:unknown, ' '}
[13080]  {:unknown, 't'}
[13081]  {:unknown, 'o'}
[13082]  {:unknown, ' '}
[13083]  {:unknown, 'y'}
[13084]  {:unknown, 'o'}
[13085]  {:unknown, 'u'}
[13086]  {:unknown, 'r'}
[13087]  {:unknown, ' '}
[13088]  {:unknown, 's'}
[13089]  {:unknown, 'o'}
[13090]  {:unknown, 'u'}
[13091]  {:unknown, 't'}
[13092]  {:unknown, 'h'}
[13093]  {:unknown, '.'}
[13094]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[13098]  {:unknown, 't'}
[13099]  {:unknown, 'h'}
[13100]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[13104]  {:unknown, 't'}
[13105]  {:unknown, 'h'}
[13106]  {:rmem, {:value, 84}, {:value, 114}}
[13109]  {:unknown, 'o'}
[13110]  {:unknown, 'p'}
[13111]  {:unknown, 'i'}
[13112]  {:unknown, 'c'}
[13113]  {:unknown, 'a'}
[13114]  {:unknown, 'l'}
[13115]  {:unknown, ' '}
[13116]  {:unknown, 'I'}
[13117]  {:unknown, 's'}
[13118]  {:unknown, 'l'}
[13119]  {:unknown, 'a'}
[13120]  {:unknown, 'n'}
[13121]  {:unknown, 'd'}
[13122]  {:unknown, [285]}
[13123]  {:unknown, 'Y'}
[13124]  {:unknown, 'o'}
[13125]  {:unknown, 'u'}
[13126]  {:unknown, ' '}
[13127]  {:unknown, 'a'}
[13128]  {:unknown, 'r'}
[13129]  {:unknown, 'e'}
[13130]  {:unknown, ' '}
[13131]  {:unknown, 'o'}
[13132]  {:unknown, 'n'}
[13133]  {:unknown, ' '}
[13134]  {:unknown, 'a'}
[13135]  {:unknown, ' '}
[13136]  {:unknown, 'n'}
[13137]  {:unknown, 'a'}
[13138]  {:unknown, 'r'}
[13139]  {:unknown, 'r'}
[13140]  {:unknown, 'o'}
[13141]  {:unknown, 'w'}
[13142]  {:unknown, ' '}
[13143]  {:unknown, 'p'}
[13144]  {:unknown, 'a'}
[13145]  {:unknown, 't'}
[13146]  {:unknown, 'h'}
[13147]  {:unknown, ' '}
[13148]  {:unknown, 'b'}
[13149]  {:unknown, 'e'}
[13150]  {:unknown, 't'}
[13151]  {:unknown, 'w'}
[13152]  {:unknown, 'e'}
[13153]  {:unknown, 'e'}
[13154]  {:unknown, 'n'}
[13155]  {:unknown, ' '}
[13156]  {:unknown, 't'}
[13157]  {:unknown, 'w'}
[13158]  {:unknown, 'o'}
[13159]  {:unknown, ' '}
[13160]  {:unknown, 's'}
[13161]  {:unknown, 't'}
[13162]  {:unknown, 'e'}
[13163]  {:unknown, 'e'}
[13164]  {:unknown, 'p'}
[13165]  {:unknown, ' '}
[13166]  {:unknown, 'r'}
[13167]  {:unknown, 'o'}
[13168]  {:unknown, 'c'}
[13169]  {:unknown, 'k'}
[13170]  {:unknown, ' '}
[13171]  {:unknown, 'f'}
[13172]  {:unknown, 'a'}
[13173]  {:unknown, 'c'}
[13174]  {:unknown, 'e'}
[13175]  {:unknown, 's'}
[13176]  {:unknown, ' '}
[13177]  {:unknown, 'w'}
[13178]  {:unknown, 'h'}
[13179]  {:unknown, 'i'}
[13180]  {:unknown, 'c'}
[13181]  {:unknown, 'h'}
[13182]  {:unknown, ' '}
[13183]  {:unknown, 'l'}
[13184]  {:unknown, 'o'}
[13185]  {:unknown, 'o'}
[13186]  {:unknown, 'k'}
[13187]  {:unknown, ' '}
[13188]  {:unknown, 'l'}
[13189]  {:unknown, 'i'}
[13190]  {:unknown, 'k'}
[13191]  {:unknown, 'e'}
[13192]  {:unknown, ' '}
[13193]  {:unknown, 't'}
[13194]  {:unknown, 'h'}
[13195]  {:unknown, 'e'}
[13196]  {:unknown, 'y'}
[13197]  {:unknown, ' '}
[13198]  {:unknown, 'h'}
[13199]  {:unknown, 'a'}
[13200]  {:unknown, 'v'}
[13201]  {:unknown, 'e'}
[13202]  {:unknown, ' '}
[13203]  {:unknown, 'b'}
[13204]  {:unknown, 'e'}
[13205]  {:unknown, 'e'}
[13206]  {:unknown, 'n'}
[13207]  {:unknown, ' '}
[13208]  {:unknown, 'h'}
[13209]  {:unknown, 'e'}
[13210]  {:unknown, 'r'}
[13211]  {:unknown, 'e'}
[13212]  {:unknown, ' '}
[13213]  {:unknown, 'f'}
[13214]  {:unknown, 'o'}
[13215]  {:unknown, 'r'}
[13216]  {:unknown, ' '}
[13217]  {:unknown, 't'}
[13218]  {:unknown, 'h'}
[13219]  {:unknown, 'o'}
[13220]  {:unknown, 'u'}
[13221]  {:unknown, 's'}
[13222]  {:unknown, 'a'}
[13223]  {:unknown, 'n'}
[13224]  {:unknown, 'd'}
[13225]  {:unknown, 's'}
[13226]  {:unknown, ' '}
[13227]  {:unknown, 'o'}
[13228]  {:unknown, 'f'}
[13229]  {:unknown, ' '}
[13230]  {:unknown, 'y'}
[13231]  {:unknown, 'e'}
[13232]  {:unknown, 'a'}
[13233]  {:unknown, 'r'}
[13234]  {:unknown, 's'}
[13235]  {:unknown, '.'}
[13236]  {:unknown, ' '}
[13237]  {:unknown, ' '}
[13238]  {:unknown, 'R'}
[13239]  {:unknown, 'a'}
[13240]  {:unknown, 'i'}
[13241]  {:unknown, 'n'}
[13242]  {:unknown, ' '}
[13243]  {:unknown, 't'}
[13244]  {:unknown, 'r'}
[13245]  {:unknown, 'i'}
[13246]  {:unknown, 'c'}
[13247]  {:unknown, 'k'}
[13248]  {:unknown, 'l'}
[13249]  {:unknown, 'e'}
[13250]  {:unknown, 's'}
[13251]  {:unknown, ' '}
[13252]  {:unknown, 'd'}
[13253]  {:unknown, 'o'}
[13254]  {:unknown, 'w'}
[13255]  {:unknown, 'n'}
[13256]  {:unknown, ' '}
[13257]  {:unknown, 't'}
[13258]  {:unknown, 'h'}
[13259]  {:unknown, 'r'}
[13260]  {:unknown, 'o'}
[13261]  {:unknown, 'u'}
[13262]  {:unknown, 'g'}
[13263]  {:unknown, 'h'}
[13264]  {:unknown, ' '}
[13265]  {:unknown, 't'}
[13266]  {:unknown, 'h'}
[13267]  {:unknown, 'e'}
[13268]  {:unknown, ' '}
[13269]  {:unknown, 'v'}
[13270]  {:unknown, 'e'}
[13271]  {:unknown, 'g'}
[13272]  {:unknown, 'e'}
[13273]  {:unknown, 't'}
[13274]  {:unknown, 'a'}
[13275]  {:unknown, 't'}
[13276]  {:unknown, 'i'}
[13277]  {:unknown, 'o'}
[13278]  {:unknown, 'n'}
[13279]  {:unknown, ' '}
[13280]  {:unknown, 'a'}
[13281]  {:unknown, 'n'}
[13282]  {:unknown, 'd'}
[13283]  {:unknown, ' '}
[13284]  {:unknown, 'm'}
[13285]  {:unknown, 'o'}
[13286]  {:unknown, 's'}
[13287]  {:unknown, 's'}
[13288]  {:unknown, ','}
[13289]  {:unknown, ' '}
[13290]  {:unknown, 'a'}
[13291]  {:unknown, 'n'}
[13292]  {:unknown, 'd'}
[13293]  {:unknown, ' '}
[13294]  {:unknown, 't'}
[13295]  {:unknown, 'h'}
[13296]  {:unknown, 'r'}
[13297]  {:unknown, 'o'}
[13298]  {:unknown, 'u'}
[13299]  {:unknown, 'g'}
[13300]  {:unknown, 'h'}
[13301]  {:unknown, ' '}
[13302]  {:unknown, 't'}
[13303]  {:unknown, 'h'}
[13304]  {:unknown, 'e'}
[13305]  {:unknown, ' '}
[13306]  {:unknown, 'l'}
[13307]  {:unknown, 'e'}
[13308]  {:unknown, 'a'}
[13309]  {:unknown, 'v'}
[13310]  {:unknown, 'e'}
[13311]  {:unknown, 's'}
[13312]  {:unknown, ' '}
[13313]  {:unknown, 'y'}
[13314]  {:unknown, 'o'}
[13315]  {:unknown, 'u'}
[13316]  {:unknown, ' '}
[13317]  {:unknown, 'c'}
[13318]  {:unknown, 'a'}
[13319]  {:unknown, 'n'}
[13320]  {:unknown, ' '}
[13321]  {:unknown, 'o'}
[13322]  {:unknown, 'c'}
[13323]  {:unknown, 'c'}
[13324]  {:unknown, 'a'}
[13325]  {:unknown, 's'}
[13326]  {:unknown, 'i'}
[13327]  {:unknown, 'o'}
[13328]  {:unknown, 'n'}
[13329]  {:unknown, 'a'}
[13330]  {:unknown, 'l'}
[13331]  {:unknown, 'l'}
[13332]  {:unknown, 'y'}
[13333]  {:unknown, ' '}
[13334]  {:unknown, 's'}
[13335]  {:unknown, 'e'}
[13336]  {:unknown, 'e'}
[13337]  {:unknown, ' '}
[13338]  {:unknown, 'a'}
[13339]  {:unknown, ' '}
[13340]  {:unknown, 's'}
[13341]  {:unknown, 'l'}
[13342]  {:unknown, 'i'}
[13343]  {:unknown, 'v'}
[13344]  {:unknown, 'e'}
[13345]  {:unknown, 'r'}
[13346]  {:unknown, ' '}
[13347]  {:unknown, 'o'}
[13348]  {:unknown, 'f'}
[13349]  {:unknown, ' '}
[13350]  {:unknown, 'l'}
[13351]  {:unknown, 'i'}
[13352]  {:unknown, 'g'}
[13353]  {:unknown, 'h'}
[13354]  {:unknown, 't'}
[13355]  {:unknown, ' '}
[13356]  {:unknown, 'h'}
[13357]  {:unknown, 'u'}
[13358]  {:unknown, 'n'}
[13359]  {:unknown, 'd'}
[13360]  {:unknown, 'r'}
[13361]  {:unknown, 'e'}
[13362]  {:unknown, 'd'}
[13363]  {:unknown, 's'}
[13364]  {:unknown, ' '}
[13365]  {:unknown, 'o'}
[13366]  {:unknown, 'f'}
[13367]  {:unknown, ' '}
[13368]  {:unknown, 'f'}
[13369]  {:unknown, 'e'}
[13370]  {:unknown, 'e'}
[13371]  {:unknown, 't'}
[13372]  {:unknown, ' '}
[13373]  {:unknown, 'a'}
[13374]  {:unknown, 'b'}
[13375]  {:unknown, 'o'}
[13376]  {:unknown, 'v'}
[13377]  {:unknown, 'e'}
[13378]  {:unknown, ' '}
[13379]  {:unknown, 'y'}
[13380]  {:unknown, 'o'}
[13381]  {:unknown, 'u'}
[13382]  {:unknown, ' '}
[13383]  {:unknown, 'w'}
[13384]  {:unknown, 'h'}
[13385]  {:unknown, 'e'}
[13386]  {:unknown, 'r'}
[13387]  {:unknown, 'e'}
[13388]  {:unknown, ' '}
[13389]  {:unknown, 't'}
[13390]  {:unknown, 'h'}
[13391]  {:unknown, 'e'}
[13392]  {:unknown, ' '}
[13393]  {:unknown, 'r'}
[13394]  {:unknown, 'o'}
[13395]  {:unknown, 'c'}
[13396]  {:unknown, 'k'}
[13397]  {:unknown, ' '}
[13398]  {:unknown, 'w'}
[13399]  {:unknown, 'a'}
[13400]  {:unknown, 'l'}
[13401]  {:unknown, 'l'}
[13402]  {:unknown, 's'}
[13403]  {:unknown, ' '}
[13404]  {:unknown, 'e'}
[13405]  {:unknown, 'n'}
[13406]  {:unknown, 'd'}
[13407]  {:unknown, '.'}
[13408]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[13412]  {:unknown, 't'}
[13413]  {:unknown, 'h'}
[13414]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[13418]  {:unknown, 't'}
[13419]  {:unknown, 'h'}
[13420]  {:rmem, {:value, 84}, {:value, 114}}
[13423]  {:unknown, 'o'}
[13424]  {:unknown, 'p'}
[13425]  {:unknown, 'i'}
[13426]  {:unknown, 'c'}
[13427]  {:unknown, 'a'}
[13428]  {:unknown, 'l'}
[13429]  {:unknown, ' '}
[13430]  {:unknown, 'I'}
[13431]  {:unknown, 's'}
[13432]  {:unknown, 'l'}
[13433]  {:unknown, 'a'}
[13434]  {:unknown, 'n'}
[13435]  {:unknown, 'd'}
[13436]  {:unknown, [245]}
[13437]  {:unknown, 'T'}
[13438]  {:unknown, 'h'}
[13439]  {:unknown, 'e'}
[13440]  {:unknown, ' '}
[13441]  {:unknown, 'n'}
[13442]  {:unknown, 'a'}
[13443]  {:unknown, 'r'}
[13444]  {:unknown, 'r'}
[13445]  {:unknown, 'o'}
[13446]  {:unknown, 'w'}
[13447]  {:unknown, ' '}
[13448]  {:unknown, 'p'}
[13449]  {:unknown, 'a'}
[13450]  {:unknown, 't'}
[13451]  {:unknown, 'h'}
[13452]  {:unknown, ' '}
[13453]  {:unknown, 's'}
[13454]  {:unknown, 'l'}
[13455]  {:unknown, 'o'}
[13456]  {:unknown, 'p'}
[13457]  {:unknown, 'e'}
[13458]  {:unknown, 's'}
[13459]  {:unknown, ' '}
[13460]  {:unknown, 'd'}
[13461]  {:unknown, 'o'}
[13462]  {:unknown, 'w'}
[13463]  {:unknown, 'n'}
[13464]  {:unknown, 'w'}
[13465]  {:unknown, 'a'}
[13466]  {:unknown, 'r'}
[13467]  {:unknown, 'd'}
[13468]  {:unknown, ' '}
[13469]  {:unknown, 't'}
[13470]  {:unknown, 'o'}
[13471]  {:unknown, ' '}
[13472]  {:unknown, 't'}
[13473]  {:unknown, 'h'}
[13474]  {:unknown, 'e'}
[13475]  {:unknown, ' '}
[13476]  {:unknown, 'n'}
[13477]  {:unknown, 'o'}
[13478]  {:unknown, 'r'}
[13479]  {:unknown, 't'}
[13480]  {:unknown, 'h'}
[13481]  {:unknown, ' '}
[13482]  {:unknown, 'a'}
[13483]  {:unknown, 'n'}
[13484]  {:unknown, 'd'}
[13485]  {:unknown, ' '}
[13486]  {:unknown, 'l'}
[13487]  {:unknown, 'e'}
[13488]  {:unknown, 'a'}
[13489]  {:unknown, 'd'}
[13490]  {:unknown, 's'}
[13491]  {:unknown, ' '}
[13492]  {:unknown, 't'}
[13493]  {:unknown, 'o'}
[13494]  {:unknown, ' '}
[13495]  {:unknown, 't'}
[13496]  {:unknown, 'h'}
[13497]  {:unknown, 'e'}
[13498]  {:unknown, ' '}
[13499]  {:unknown, 'm'}
[13500]  {:unknown, 'o'}
[13501]  {:unknown, 'u'}
[13502]  {:unknown, 't'}
[13503]  {:unknown, 'h'}
[13504]  {:unknown, ' '}
[13505]  {:unknown, 'o'}
[13506]  {:unknown, 'f'}
[13507]  {:unknown, ' '}
[13508]  {:unknown, 'a'}
[13509]  {:unknown, ' '}
[13510]  {:unknown, 's'}
[13511]  {:unknown, 'm'}
[13512]  {:unknown, 'a'}
[13513]  {:unknown, 'l'}
[13514]  {:unknown, 'l'}
[13515]  {:unknown, ' '}
[13516]  {:unknown, 'c'}
[13517]  {:unknown, 'a'}
[13518]  {:unknown, 'v'}
[13519]  {:unknown, 'e'}
[13520]  {:unknown, '.'}
[13521]  {:unknown, ' '}
[13522]  {:unknown, ' '}
[13523]  {:unknown, 'A'}
[13524]  {:unknown, ' '}
[13525]  {:unknown, 's'}
[13526]  {:unknown, 'i'}
[13527]  {:unknown, 'g'}
[13528]  {:unknown, 'n'}
[13529]  {:unknown, ' '}
[13530]  {:unknown, 'n'}
[13531]  {:unknown, 'e'}
[13532]  {:unknown, 'a'}
[13533]  {:unknown, 'r'}
[13534]  {:unknown, 'b'}
[13535]  {:unknown, 'y'}
[13536]  {:unknown, ' '}
[13537]  {:unknown, 'r'}
[13538]  {:unknown, 'e'}
[13539]  {:unknown, 'a'}
[13540]  {:unknown, 'd'}
[13541]  {:unknown, 's'}
[13542]  {:unknown, ' '}
[13543]  {:unknown, '"'}
[13544]  {:unknown, 'T'}
[13545]  {:unknown, 'r'}
[13546]  {:unknown, 'e'}
[13547]  {:unknown, 'a'}
[13548]  {:unknown, 's'}
[13549]  {:unknown, 'u'}
[13550]  {:unknown, 'r'}
[13551]  {:unknown, 'e'}
[13552]  {:unknown, ' '}
[13553]  {:unknown, 'V'}
[13554]  {:unknown, 'a'}
[13555]  {:unknown, 'u'}
[13556]  {:unknown, 'l'}
[13557]  {:unknown, 't'}
[13558]  {:unknown, ' '}
[13559]  {:unknown, 'A'}
[13560]  {:unknown, 'c'}
[13561]  {:unknown, 'c'}
[13562]  {:unknown, 'e'}
[13563]  {:unknown, 's'}
[13564]  {:unknown, 's'}
[13565]  {:unknown, '"'}
[13566]  {:unknown, ','}
[13567]  {:unknown, ' '}
[13568]  {:unknown, 'b'}
[13569]  {:unknown, 'u'}
[13570]  {:unknown, 't'}
[13571]  {:unknown, ' '}
[13572]  {:unknown, 'd'}
[13573]  {:unknown, 'i'}
[13574]  {:unknown, 'f'}
[13575]  {:unknown, 'f'}
[13576]  {:unknown, 'e'}
[13577]  {:unknown, 'r'}
[13578]  {:unknown, 'e'}
[13579]  {:unknown, 'n'}
[13580]  {:unknown, 't'}
[13581]  {:unknown, ' '}
[13582]  {:unknown, 'h'}
[13583]  {:unknown, 'a'}
[13584]  {:unknown, 'n'}
[13585]  {:unknown, 'd'}
[13586]  {:unknown, 'w'}
[13587]  {:unknown, 'r'}
[13588]  {:unknown, 'i'}
[13589]  {:unknown, 't'}
[13590]  {:unknown, 'i'}
[13591]  {:unknown, 'n'}
[13592]  {:unknown, 'g'}
[13593]  {:unknown, ' '}
[13594]  {:unknown, 'h'}
[13595]  {:unknown, 'a'}
[13596]  {:unknown, 's'}
[13597]  {:unknown, ' '}
[13598]  {:unknown, 'c'}
[13599]  {:unknown, 'r'}
[13600]  {:unknown, 'o'}
[13601]  {:unknown, 's'}
[13602]  {:unknown, 's'}
[13603]  {:unknown, 'e'}
[13604]  {:unknown, 'd'}
[13605]  {:unknown, ' '}
[13606]  {:unknown, 't'}
[13607]  {:unknown, 'h'}
[13608]  {:unknown, 'i'}
[13609]  {:unknown, 's'}
[13610]  {:unknown, ' '}
[13611]  {:unknown, 'o'}
[13612]  {:unknown, 'u'}
[13613]  {:unknown, 't'}
[13614]  {:unknown, ' '}
[13615]  {:unknown, 'a'}
[13616]  {:unknown, 'n'}
[13617]  {:unknown, 'd'}
[13618]  {:unknown, ' '}
[13619]  {:unknown, 'w'}
[13620]  {:unknown, 'r'}
[13621]  {:unknown, 'i'}
[13622]  {:unknown, 't'}
[13623]  {:unknown, 't'}
[13624]  {:unknown, 'e'}
[13625]  {:unknown, 'n'}
[13626]  {:unknown, ' '}
[13627]  {:unknown, '"'}
[13628]  {:unknown, 'L'}
[13629]  {:unknown, 'a'}
[13630]  {:unknown, 'i'}
[13631]  {:unknown, 'r'}
[13632]  {:unknown, ' '}
[13633]  {:unknown, 'o'}
[13634]  {:unknown, 'f'}
[13635]  {:unknown, ' '}
[13636]  {:unknown, 'H'}
[13637]  {:unknown, 'o'}
[13638]  {:unknown, 'r'}
[13639]  {:unknown, 'r'}
[13640]  {:unknown, 'i'}
[13641]  {:unknown, 'b'}
[13642]  {:unknown, 'l'}
[13643]  {:unknown, 'e'}
[13644]  {:unknown, ' '}
[13645]  {:unknown, 'M'}
[13646]  {:unknown, 'o'}
[13647]  {:unknown, 'n'}
[13648]  {:unknown, 's'}
[13649]  {:unknown, 't'}
[13650]  {:unknown, 'e'}
[13651]  {:unknown, 'r'}
[13652]  {:unknown, '!'}
[13653]  {:unknown, ' '}
[13654]  {:unknown, ' '}
[13655]  {:unknown, 'A'}
[13656]  {:unknown, 'l'}
[13657]  {:unknown, 'l'}
[13658]  {:unknown, ' '}
[13659]  {:unknown, 'n'}
[13660]  {:unknown, 'o'}
[13661]  {:unknown, 'n'}
[13662]  {:unknown, '-'}
[13663]  {:unknown, 'p'}
[13664]  {:unknown, 'i'}
[13665]  {:unknown, 'r'}
[13666]  {:unknown, 'a'}
[13667]  {:unknown, 't'}
[13668]  {:unknown, 'e'}
[13669]  {:unknown, 's'}
[13670]  {:unknown, ' '}
[13671]  {:unknown, 'k'}
[13672]  {:unknown, 'e'}
[13673]  {:unknown, 'e'}
[13674]  {:unknown, 'p'}
[13675]  {:unknown, ' '}
[13676]  {:unknown, 'o'}
[13677]  {:unknown, 'u'}
[13678]  {:unknown, 't'}
[13679]  {:unknown, '!'}
[13680]  {:unknown, '"'}
[13681]  {:unknown, '.'}
[13682]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[13686]  {:unknown, 't'}
[13687]  {:unknown, 'h'}
[13688]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[13692]  {:unknown, 't'}
[13693]  {:unknown, 'h'}
[13694]  {:or, {:value, 84}, {:value, 114}, {:value, 111}}
[13698]  {:unknown, 'p'}
[13699]  {:unknown, 'i'}
[13700]  {:unknown, 'c'}
[13701]  {:unknown, 'a'}
[13702]  {:unknown, 'l'}
[13703]  {:unknown, ' '}
[13704]  {:unknown, 'C'}
[13705]  {:unknown, 'a'}
[13706]  {:unknown, 'v'}
[13707]  {:unknown, 'e'}
[13708]  {:unknown, [220]}
[13709]  {:unknown, 'Y'}
[13710]  {:unknown, 'o'}
[13711]  {:unknown, 'u'}
[13712]  {:unknown, ' '}
[13713]  {:unknown, 's'}
[13714]  {:unknown, 't'}
[13715]  {:unknown, 'a'}
[13716]  {:unknown, 'n'}
[13717]  {:unknown, 'd'}
[13718]  {:unknown, ' '}
[13719]  {:unknown, 'a'}
[13720]  {:unknown, 't'}
[13721]  {:unknown, ' '}
[13722]  {:unknown, 't'}
[13723]  {:unknown, 'h'}
[13724]  {:unknown, 'e'}
[13725]  {:unknown, ' '}
[13726]  {:unknown, 'e'}
[13727]  {:unknown, 'n'}
[13728]  {:unknown, 't'}
[13729]  {:unknown, 'r'}
[13730]  {:unknown, 'a'}
[13731]  {:unknown, 'n'}
[13732]  {:unknown, 'c'}
[13733]  {:unknown, 'e'}
[13734]  {:unknown, ' '}
[13735]  {:unknown, 't'}
[13736]  {:unknown, 'o'}
[13737]  {:unknown, ' '}
[13738]  {:unknown, 'a'}
[13739]  {:unknown, ' '}
[13740]  {:unknown, 'n'}
[13741]  {:unknown, 'a'}
[13742]  {:unknown, 't'}
[13743]  {:unknown, 'u'}
[13744]  {:unknown, 'r'}
[13745]  {:unknown, 'a'}
[13746]  {:unknown, 'l'}
[13747]  {:unknown, ' '}
[13748]  {:unknown, 'c'}
[13749]  {:unknown, 'a'}
[13750]  {:unknown, 'v'}
[13751]  {:unknown, 'e'}
[13752]  {:unknown, ' '}
[13753]  {:unknown, 'w'}
[13754]  {:unknown, 'h'}
[13755]  {:unknown, 'i'}
[13756]  {:unknown, 'c'}
[13757]  {:unknown, 'h'}
[13758]  {:unknown, ' '}
[13759]  {:unknown, 'l'}
[13760]  {:unknown, 'o'}
[13761]  {:unknown, 'o'}
[13762]  {:unknown, 'k'}
[13763]  {:unknown, 's'}
[13764]  {:unknown, ' '}
[13765]  {:unknown, 'l'}
[13766]  {:unknown, 'i'}
[13767]  {:unknown, 'k'}
[13768]  {:unknown, 'e'}
[13769]  {:unknown, ' '}
[13770]  {:unknown, 'i'}
[13771]  {:unknown, 't'}
[13772]  {:unknown, ' '}
[13773]  {:unknown, 'h'}
[13774]  {:unknown, 'a'}
[13775]  {:unknown, 's'}
[13776]  {:unknown, 'n'}
[13777]  {:unknown, '\''}
[13778]  {:unknown, 't'}
[13779]  {:unknown, ' '}
[13780]  {:unknown, 'b'}
[13781]  {:unknown, 'e'}
[13782]  {:unknown, 'e'}
[13783]  {:unknown, 'n'}
[13784]  {:unknown, ' '}
[13785]  {:unknown, 'v'}
[13786]  {:unknown, 'i'}
[13787]  {:unknown, 's'}
[13788]  {:unknown, 'i'}
[13789]  {:unknown, 't'}
[13790]  {:unknown, 'e'}
[13791]  {:unknown, 'd'}
[13792]  {:unknown, ' '}
[13793]  {:unknown, 'i'}
[13794]  {:unknown, 'n'}
[13795]  {:unknown, ' '}
[13796]  {:unknown, 'q'}
[13797]  {:unknown, 'u'}
[13798]  {:unknown, 'i'}
[13799]  {:unknown, 't'}
[13800]  {:unknown, 'e'}
[13801]  {:unknown, ' '}
[13802]  {:unknown, 's'}
[13803]  {:unknown, 'o'}
[13804]  {:unknown, 'm'}
[13805]  {:unknown, 'e'}
[13806]  {:unknown, ' '}
[13807]  {:unknown, 't'}
[13808]  {:unknown, 'i'}
[13809]  {:unknown, 'm'}
[13810]  {:unknown, 'e'}
[13811]  {:unknown, '.'}
[13812]  {:unknown, ' '}
[13813]  {:unknown, ' '}
[13814]  {:unknown, 'L'}
[13815]  {:unknown, 'i'}
[13816]  {:unknown, 'g'}
[13817]  {:unknown, 'h'}
[13818]  {:unknown, 't'}
[13819]  {:unknown, ' '}
[13820]  {:unknown, 'p'}
[13821]  {:unknown, 'o'}
[13822]  {:unknown, 'u'}
[13823]  {:unknown, 'r'}
[13824]  {:unknown, 's'}
[13825]  {:unknown, ' '}
[13826]  {:unknown, 'i'}
[13827]  {:unknown, 'n'}
[13828]  {:unknown, ' '}
[13829]  {:unknown, 't'}
[13830]  {:unknown, 'h'}
[13831]  {:unknown, 'r'}
[13832]  {:unknown, 'o'}
[13833]  {:unknown, 'u'}
[13834]  {:unknown, 'g'}
[13835]  {:unknown, 'h'}
[13836]  {:unknown, ' '}
[13837]  {:unknown, 't'}
[13838]  {:unknown, 'h'}
[13839]  {:unknown, 'e'}
[13840]  {:unknown, ' '}
[13841]  {:unknown, 'o'}
[13842]  {:unknown, 'p'}
[13843]  {:unknown, 'e'}
[13844]  {:unknown, 'n'}
[13845]  {:unknown, 'i'}
[13846]  {:unknown, 'n'}
[13847]  {:unknown, 'g'}
[13848]  {:unknown, ' '}
[13849]  {:unknown, 't'}
[13850]  {:unknown, 'o'}
[13851]  {:unknown, ' '}
[13852]  {:unknown, 't'}
[13853]  {:unknown, 'h'}
[13854]  {:unknown, 'e'}
[13855]  {:unknown, ' '}
[13856]  {:unknown, 's'}
[13857]  {:unknown, 'o'}
[13858]  {:unknown, 'u'}
[13859]  {:unknown, 't'}
[13860]  {:unknown, 'h'}
[13861]  {:unknown, ','}
[13862]  {:unknown, ' '}
[13863]  {:unknown, 'w'}
[13864]  {:unknown, 'h'}
[13865]  {:unknown, 'i'}
[13866]  {:unknown, 'l'}
[13867]  {:unknown, 'e'}
[13868]  {:unknown, ' '}
[13869]  {:unknown, 'f'}
[13870]  {:unknown, 'i'}
[13871]  {:unknown, 'r'}
[13872]  {:unknown, 'e'}
[13873]  {:unknown, 'f'}
[13874]  {:unknown, 'l'}
[13875]  {:unknown, 'i'}
[13876]  {:unknown, 'e'}
[13877]  {:unknown, 's'}
[13878]  {:unknown, ' '}
[13879]  {:unknown, 'l'}
[13880]  {:unknown, 'i'}
[13881]  {:unknown, 'g'}
[13882]  {:unknown, 'h'}
[13883]  {:unknown, 't'}
[13884]  {:unknown, ' '}
[13885]  {:unknown, 't'}
[13886]  {:unknown, 'h'}
[13887]  {:unknown, 'e'}
[13888]  {:unknown, ' '}
[13889]  {:unknown, 'p'}
[13890]  {:unknown, 'a'}
[13891]  {:unknown, 't'}
[13892]  {:unknown, 'h'}
[13893]  {:unknown, ' '}
[13894]  {:unknown, 'f'}
[13895]  {:unknown, 'u'}
[13896]  {:unknown, 'r'}
[13897]  {:unknown, 't'}
[13898]  {:unknown, 'h'}
[13899]  {:unknown, 'e'}
[13900]  {:unknown, 'r'}
[13901]  {:unknown, ' '}
[13902]  {:unknown, 'i'}
[13903]  {:unknown, 'n'}
[13904]  {:unknown, 't'}
[13905]  {:unknown, 'o'}
[13906]  {:unknown, ' '}
[13907]  {:unknown, 't'}
[13908]  {:unknown, 'h'}
[13909]  {:unknown, 'e'}
[13910]  {:unknown, ' '}
[13911]  {:unknown, 'c'}
[13912]  {:unknown, 'a'}
[13913]  {:unknown, 'v'}
[13914]  {:unknown, 'e'}
[13915]  {:unknown, ' '}
[13916]  {:unknown, 't'}
[13917]  {:unknown, 'o'}
[13918]  {:unknown, ' '}
[13919]  {:unknown, 't'}
[13920]  {:unknown, 'h'}
[13921]  {:unknown, 'e'}
[13922]  {:unknown, ' '}
[13923]  {:unknown, 'n'}
[13924]  {:unknown, 'o'}
[13925]  {:unknown, 'r'}
[13926]  {:unknown, 't'}
[13927]  {:unknown, 'h'}
[13928]  {:unknown, '.'}
[13929]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[13933]  {:unknown, 't'}
[13934]  {:unknown, 'h'}
[13935]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[13939]  {:unknown, 't'}
[13940]  {:unknown, 'h'}
[13941]  {:or, {:value, 84}, {:value, 114}, {:value, 111}}
[13945]  {:unknown, 'p'}
[13946]  {:unknown, 'i'}
[13947]  {:unknown, 'c'}
[13948]  {:unknown, 'a'}
[13949]  {:unknown, 'l'}
[13950]  {:unknown, ' '}
[13951]  {:unknown, 'C'}
[13952]  {:unknown, 'a'}
[13953]  {:unknown, 'v'}
[13954]  {:unknown, 'e'}
[13955]  {:unknown, 'z'}
[13956]  {:unknown, 'F'}
[13957]  {:unknown, 'i'}
[13958]  {:unknown, 'r'}
[13959]  {:unknown, 'e'}
[13960]  {:unknown, 'f'}
[13961]  {:unknown, 'l'}
[13962]  {:unknown, 'i'}
[13963]  {:unknown, 'e'}
[13964]  {:unknown, 's'}
[13965]  {:unknown, ' '}
[13966]  {:unknown, 's'}
[13967]  {:unknown, 'l'}
[13968]  {:unknown, 'o'}
[13969]  {:unknown, 'w'}
[13970]  {:unknown, 'l'}
[13971]  {:unknown, 'y'}
[13972]  {:unknown, ' '}
[13973]  {:unknown, 'd'}
[13974]  {:unknown, 'r'}
[13975]  {:unknown, 'i'}
[13976]  {:unknown, 'f'}
[13977]  {:unknown, 't'}
[13978]  {:unknown, ' '}
[13979]  {:unknown, 'a'}
[13980]  {:unknown, 'r'}
[13981]  {:unknown, 'o'}
[13982]  {:unknown, 'u'}
[13983]  {:unknown, 'n'}
[13984]  {:unknown, 'd'}
[13985]  {:unknown, ' '}
[13986]  {:unknown, 'y'}
[13987]  {:unknown, 'o'}
[13988]  {:unknown, 'u'}
[13989]  {:unknown, ' '}
[13990]  {:unknown, 'a'}
[13991]  {:unknown, 'n'}
[13992]  {:unknown, 'd'}
[13993]  {:unknown, ' '}
[13994]  {:unknown, 'l'}
[13995]  {:unknown, 'i'}
[13996]  {:unknown, 'g'}
[13997]  {:unknown, 'h'}
[13998]  {:unknown, 't'}
[13999]  {:unknown, ' '}
[14000]  {:unknown, 't'}
[14001]  {:unknown, 'h'}
[14002]  {:unknown, 'e'}
[14003]  {:unknown, ' '}
[14004]  {:unknown, 't'}
[14005]  {:unknown, 'u'}
[14006]  {:unknown, 'n'}
[14007]  {:unknown, 'n'}
[14008]  {:unknown, 'e'}
[14009]  {:unknown, 'l'}
[14010]  {:unknown, ','}
[14011]  {:unknown, ' '}
[14012]  {:unknown, 'w'}
[14013]  {:unknown, 'h'}
[14014]  {:unknown, 'i'}
[14015]  {:unknown, 'c'}
[14016]  {:unknown, 'h'}
[14017]  {:unknown, ' '}
[14018]  {:unknown, 's'}
[14019]  {:unknown, 'e'}
[14020]  {:unknown, 'e'}
[14021]  {:unknown, 'm'}
[14022]  {:unknown, 's'}
[14023]  {:unknown, ' '}
[14024]  {:unknown, 't'}
[14025]  {:unknown, 'o'}
[14026]  {:unknown, ' '}
[14027]  {:unknown, 'g'}
[14028]  {:unknown, 'e'}
[14029]  {:unknown, 't'}
[14030]  {:unknown, ' '}
[14031]  {:unknown, 'b'}
[14032]  {:unknown, 'r'}
[14033]  {:unknown, 'i'}
[14034]  {:unknown, 'g'}
[14035]  {:unknown, 'h'}
[14036]  {:unknown, 't'}
[14037]  {:unknown, 'e'}
[14038]  {:unknown, 'r'}
[14039]  {:unknown, ' '}
[14040]  {:unknown, 't'}
[14041]  {:unknown, 'o'}
[14042]  {:unknown, ' '}
[14043]  {:unknown, 't'}
[14044]  {:unknown, 'h'}
[14045]  {:unknown, 'e'}
[14046]  {:unknown, ' '}
[14047]  {:unknown, 's'}
[14048]  {:unknown, 'o'}
[14049]  {:unknown, 'u'}
[14050]  {:unknown, 't'}
[14051]  {:unknown, 'h'}
[14052]  {:unknown, ','}
[14053]  {:unknown, ' '}
[14054]  {:unknown, 'b'}
[14055]  {:unknown, 'u'}
[14056]  {:unknown, 't'}
[14057]  {:unknown, ' '}
[14058]  {:unknown, 'd'}
[14059]  {:unknown, 'i'}
[14060]  {:unknown, 'm'}
[14061]  {:unknown, 'm'}
[14062]  {:unknown, 'e'}
[14063]  {:unknown, 'r'}
[14064]  {:unknown, ' '}
[14065]  {:unknown, 't'}
[14066]  {:unknown, 'o'}
[14067]  {:unknown, ' '}
[14068]  {:unknown, 't'}
[14069]  {:unknown, 'h'}
[14070]  {:unknown, 'e'}
[14071]  {:unknown, ' '}
[14072]  {:unknown, 'n'}
[14073]  {:unknown, 'o'}
[14074]  {:unknown, 'r'}
[14075]  {:unknown, 't'}
[14076]  {:unknown, 'h'}
[14077]  {:unknown, '.'}
[14078]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[14082]  {:unknown, 't'}
[14083]  {:unknown, 'h'}
[14084]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[14088]  {:unknown, 't'}
[14089]  {:unknown, 'h'}
[14090]  {:or, {:value, 84}, {:value, 114}, {:value, 111}}
[14094]  {:unknown, 'p'}
[14095]  {:unknown, 'i'}
[14096]  {:unknown, 'c'}
[14097]  {:unknown, 'a'}
[14098]  {:unknown, 'l'}
[14099]  {:unknown, ' '}
[14100]  {:unknown, 'C'}
[14101]  {:unknown, 'a'}
[14102]  {:unknown, 'v'}
[14103]  {:unknown, 'e'}
[14104]  {:unknown, [322]}
[14105]  {:unknown, 'T'}
[14106]  {:unknown, 'h'}
[14107]  {:unknown, 'e'}
[14108]  {:unknown, ' '}
[14109]  {:unknown, 'c'}
[14110]  {:unknown, 'a'}
[14111]  {:unknown, 'v'}
[14112]  {:unknown, 'e'}
[14113]  {:unknown, ' '}
[14114]  {:unknown, 'i'}
[14115]  {:unknown, 's'}
[14116]  {:unknown, ' '}
[14117]  {:unknown, 'a'}
[14118]  {:unknown, ' '}
[14119]  {:unknown, 'l'}
[14120]  {:unknown, 'i'}
[14121]  {:unknown, 't'}
[14122]  {:unknown, 't'}
[14123]  {:unknown, 'l'}
[14124]  {:unknown, 'e'}
[14125]  {:unknown, ' '}
[14126]  {:unknown, 'w'}
[14127]  {:unknown, 'i'}
[14128]  {:unknown, 'd'}
[14129]  {:unknown, 'e'}
[14130]  {:unknown, 'r'}
[14131]  {:unknown, ' '}
[14132]  {:unknown, 'h'}
[14133]  {:unknown, 'e'}
[14134]  {:unknown, 'r'}
[14135]  {:unknown, 'e'}
[14136]  {:unknown, '.'}
[14137]  {:unknown, ' '}
[14138]  {:unknown, ' '}
[14139]  {:unknown, 'Y'}
[14140]  {:unknown, 'o'}
[14141]  {:unknown, 'u'}
[14142]  {:unknown, ' '}
[14143]  {:unknown, 'f'}
[14144]  {:unknown, 'i'}
[14145]  {:unknown, 'n'}
[14146]  {:unknown, 'd'}
[14147]  {:unknown, ' '}
[14148]  {:unknown, 't'}
[14149]  {:unknown, 'h'}
[14150]  {:unknown, 'e'}
[14151]  {:unknown, ' '}
[14152]  {:unknown, 'c'}
[14153]  {:unknown, 'o'}
[14154]  {:unknown, 'b'}
[14155]  {:unknown, 'w'}
[14156]  {:unknown, 'e'}
[14157]  {:unknown, 'b'}
[14158]  {:unknown, '-'}
[14159]  {:unknown, 'e'}
[14160]  {:unknown, 'n'}
[14161]  {:unknown, 'c'}
[14162]  {:unknown, 'r'}
[14163]  {:unknown, 'u'}
[14164]  {:unknown, 's'}
[14165]  {:unknown, 't'}
[14166]  {:unknown, 'e'}
[14167]  {:unknown, 'd'}
[14168]  {:unknown, ' '}
[14169]  {:unknown, 'r'}
[14170]  {:unknown, 'e'}
[14171]  {:unknown, 'm'}
[14172]  {:unknown, 'a'}
[14173]  {:unknown, 'i'}
[14174]  {:unknown, 'n'}
[14175]  {:unknown, 's'}
[14176]  {:unknown, ' '}
[14177]  {:unknown, 'o'}
[14178]  {:unknown, 'f'}
[14179]  {:unknown, ' '}
[14180]  {:unknown, 'a'}
[14181]  {:unknown, ' '}
[14182]  {:unknown, 's'}
[14183]  {:unknown, 'm'}
[14184]  {:unknown, 'a'}
[14185]  {:unknown, 'l'}
[14186]  {:unknown, 'l'}
[14187]  {:unknown, ' '}
[14188]  {:unknown, 'c'}
[14189]  {:unknown, 'a'}
[14190]  {:unknown, 'm'}
[14191]  {:unknown, 'p'}
[14192]  {:unknown, ','}
[14193]  {:unknown, ' '}
[14194]  {:unknown, 'a'}
[14195]  {:unknown, 'n'}
[14196]  {:unknown, 'd'}
[14197]  {:unknown, ' '}
[14198]  {:unknown, 'a'}
[14199]  {:unknown, 'l'}
[14200]  {:unknown, 't'}
[14201]  {:unknown, 'h'}
[14202]  {:unknown, 'o'}
[14203]  {:unknown, 'u'}
[14204]  {:unknown, 'g'}
[14205]  {:unknown, 'h'}
[14206]  {:unknown, ' '}
[14207]  {:unknown, 'y'}
[14208]  {:unknown, 'o'}
[14209]  {:unknown, 'u'}
[14210]  {:unknown, ' '}
[14211]  {:unknown, 'd'}
[14212]  {:unknown, 'o'}
[14213]  {:unknown, 'n'}
[14214]  {:unknown, '\''}
[14215]  {:unknown, 't'}
[14216]  {:unknown, ' '}
[14217]  {:unknown, 's'}
[14218]  {:unknown, 'u'}
[14219]  {:unknown, 's'}
[14220]  {:unknown, 'p'}
[14221]  {:unknown, 'e'}
[14222]  {:unknown, 'c'}
[14223]  {:unknown, 't'}
[14224]  {:unknown, ' '}
[14225]  {:unknown, 't'}
[14226]  {:unknown, 'h'}
[14227]  {:unknown, 'e'}
[14228]  {:unknown, ' '}
[14229]  {:unknown, 'b'}
[14230]  {:unknown, 'r'}
[14231]  {:unknown, 'o'}
[14232]  {:unknown, 'k'}
[14233]  {:unknown, 'e'}
[14234]  {:unknown, 'n'}
[14235]  {:unknown, ' '}
[14236]  {:unknown, 'p'}
[14237]  {:unknown, 'i'}
[14238]  {:unknown, 'e'}
[14239]  {:unknown, 'c'}
[14240]  {:unknown, 'e'}
[14241]  {:unknown, 's'}
[14242]  {:unknown, ' '}
[14243]  {:unknown, 'o'}
[14244]  {:unknown, 'f'}
[14245]  {:unknown, ' '}
[14246]  {:unknown, 't'}
[14247]  {:unknown, 'a'}
[14248]  {:unknown, 'b'}
[14249]  {:unknown, 'l'}
[14250]  {:unknown, 'e'}
[14251]  {:unknown, 's'}
[14252]  {:unknown, ' '}
[14253]  {:unknown, 'a'}
[14254]  {:unknown, 'n'}
[14255]  {:unknown, 'd'}
[14256]  {:unknown, ' '}
[14257]  {:unknown, 'c'}
[14258]  {:unknown, 'h'}
[14259]  {:unknown, 'a'}
[14260]  {:unknown, 'i'}
[14261]  {:unknown, 'r'}
[14262]  {:unknown, 's'}
[14263]  {:unknown, ' '}
[14264]  {:unknown, 'w'}
[14265]  {:unknown, 'i'}
[14266]  {:unknown, 'l'}
[14267]  {:unknown, 'l'}
[14268]  {:unknown, ' '}
[14269]  {:unknown, 'p'}
[14270]  {:unknown, 'r'}
[14271]  {:unknown, 'o'}
[14272]  {:unknown, 'v'}
[14273]  {:unknown, 'e'}
[14274]  {:unknown, ' '}
[14275]  {:unknown, 'u'}
[14276]  {:unknown, 's'}
[14277]  {:unknown, 'e'}
[14278]  {:unknown, 'f'}
[14279]  {:unknown, 'u'}
[14280]  {:unknown, 'l'}
[14281]  {:unknown, ' '}
[14282]  {:unknown, 't'}
[14283]  {:unknown, 'o'}
[14284]  {:unknown, ' '}
[14285]  {:unknown, 'y'}
[14286]  {:unknown, 'o'}
[14287]  {:unknown, 'u'}
[14288]  {:unknown, 'r'}
[14289]  {:unknown, ' '}
[14290]  {:unknown, 'q'}
[14291]  {:unknown, 'u'}
[14292]  {:unknown, 'e'}
[14293]  {:unknown, 's'}
[14294]  {:unknown, 't'}
[14295]  {:unknown, ','}
[14296]  {:unknown, ' '}
[14297]  {:unknown, 't'}
[14298]  {:unknown, 'h'}
[14299]  {:unknown, 'e'}
[14300]  {:unknown, ' '}
[14301]  {:unknown, 'f'}
[14302]  {:unknown, 'i'}
[14303]  {:unknown, 'r'}
[14304]  {:unknown, 'e'}
[14305]  {:unknown, 'f'}
[14306]  {:unknown, 'l'}
[14307]  {:unknown, 'i'}
[14308]  {:unknown, 'e'}
[14309]  {:unknown, 's'}
[14310]  {:unknown, ' '}
[14311]  {:unknown, 's'}
[14312]  {:unknown, 'e'}
[14313]  {:unknown, 'e'}
[14314]  {:unknown, 'm'}
[14315]  {:unknown, ' '}
[14316]  {:unknown, 't'}
[14317]  {:unknown, 'o'}
[14318]  {:unknown, ' '}
[14319]  {:unknown, 'l'}
[14320]  {:unknown, 'i'}
[14321]  {:unknown, 'k'}
[14322]  {:unknown, 'e'}
[14323]  {:unknown, ' '}
[14324]  {:unknown, 'u'}
[14325]  {:unknown, 's'}
[14326]  {:unknown, 'i'}
[14327]  {:unknown, 'n'}
[14328]  {:unknown, 'g'}
[14329]  {:unknown, ' '}
[14330]  {:unknown, 't'}
[14331]  {:unknown, 'h'}
[14332]  {:unknown, 'e'}
[14333]  {:unknown, ' '}
[14334]  {:unknown, 'd'}
[14335]  {:unknown, 'e'}
[14336]  {:unknown, 'b'}
[14337]  {:unknown, 'r'}
[14338]  {:unknown, 'i'}
[14339]  {:unknown, 's'}
[14340]  {:unknown, ' '}
[14341]  {:unknown, 'a'}
[14342]  {:unknown, 's'}
[14343]  {:unknown, ' '}
[14344]  {:unknown, 'a'}
[14345]  {:unknown, ' '}
[14346]  {:unknown, 's'}
[14347]  {:unknown, 'h'}
[14348]  {:unknown, 'e'}
[14349]  {:unknown, 'l'}
[14350]  {:unknown, 't'}
[14351]  {:unknown, 'e'}
[14352]  {:unknown, 'r'}
[14353]  {:unknown, '.'}
[14354]  {:unknown, ' '}
[14355]  {:unknown, ' '}
[14356]  {:unknown, 'A'}
[14357]  {:unknown, ' '}
[14358]  {:unknown, 'p'}
[14359]  {:unknown, 'a'}
[14360]  {:unknown, 's'}
[14361]  {:unknown, 's'}
[14362]  {:unknown, 'a'}
[14363]  {:unknown, 'g'}
[14364]  {:unknown, 'e'}
[14365]  {:unknown, 'w'}
[14366]  {:unknown, 'a'}
[14367]  {:unknown, 'y'}
[14368]  {:unknown, ' '}
[14369]  {:unknown, 'l'}
[14370]  {:unknown, 'e'}
[14371]  {:unknown, 'a'}
[14372]  {:unknown, 'd'}
[14373]  {:unknown, 's'}
[14374]  {:unknown, ' '}
[14375]  {:unknown, 'n'}
[14376]  {:unknown, 'o'}
[14377]  {:unknown, 'r'}
[14378]  {:unknown, 't'}
[14379]  {:unknown, 'h'}
[14380]  {:unknown, ' '}
[14381]  {:unknown, 'a'}
[14382]  {:unknown, 'n'}
[14383]  {:unknown, 'd'}
[14384]  {:unknown, ' '}
[14385]  {:unknown, 's'}
[14386]  {:unknown, 'o'}
[14387]  {:unknown, 'u'}
[14388]  {:unknown, 't'}
[14389]  {:unknown, 'h'}
[14390]  {:unknown, ','}
[14391]  {:unknown, ' '}
[14392]  {:unknown, 'a'}
[14393]  {:unknown, 'n'}
[14394]  {:unknown, 'd'}
[14395]  {:unknown, ' '}
[14396]  {:unknown, 't'}
[14397]  {:unknown, 'h'}
[14398]  {:unknown, 'e'}
[14399]  {:unknown, 'r'}
[14400]  {:unknown, 'e'}
[14401]  {:unknown, ' '}
[14402]  {:unknown, 'i'}
[14403]  {:unknown, 's'}
[14404]  {:unknown, ' '}
[14405]  {:unknown, 'a'}
[14406]  {:unknown, 'n'}
[14407]  {:unknown, ' '}
[14408]  {:unknown, 'a'}
[14409]  {:unknown, 'l'}
[14410]  {:unknown, 'c'}
[14411]  {:unknown, 'o'}
[14412]  {:unknown, 'v'}
[14413]  {:unknown, 'e'}
[14414]  {:unknown, ' '}
[14415]  {:unknown, 't'}
[14416]  {:unknown, 'o'}
[14417]  {:unknown, ' '}
[14418]  {:unknown, 't'}
[14419]  {:unknown, 'h'}
[14420]  {:unknown, 'e'}
[14421]  {:unknown, ' '}
[14422]  {:unknown, 'e'}
[14423]  {:unknown, 'a'}
[14424]  {:unknown, 's'}
[14425]  {:unknown, 't'}
[14426]  {:unknown, '.'}
[14427]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[14431]  {:unknown, 't'}
[14432]  {:unknown, 'h'}
[14433]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[14437]  {:unknown, 't'}
[14438]  {:unknown, 'h'}
[14439]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[14443]  {:unknown, 't'}
[14444]  {:in, {:value, 84}}
[14446]  {:unknown, 'r'}
[14447]  {:unknown, 'o'}
[14448]  {:unknown, 'p'}
[14449]  {:unknown, 'i'}
[14450]  {:unknown, 'c'}
[14451]  {:unknown, 'a'}
[14452]  {:unknown, 'l'}
[14453]  {:unknown, ' '}
[14454]  {:unknown, 'C'}
[14455]  {:unknown, 'a'}
[14456]  {:unknown, 'v'}
[14457]  {:unknown, 'e'}
[14458]  {:unknown, ' '}
[14459]  {:unknown, 'A'}
[14460]  {:unknown, 'l'}
[14461]  {:unknown, 'c'}
[14462]  {:unknown, 'o'}
[14463]  {:unknown, 'v'}
[14464]  {:unknown, 'e'}
[14465]  {:unknown, [163]}
[14466]  {:unknown, 'A'}
[14467]  {:unknown, 't'}
[14468]  {:unknown, ' '}
[14469]  {:unknown, 't'}
[14470]  {:unknown, 'h'}
[14471]  {:unknown, 'e'}
[14472]  {:unknown, ' '}
[14473]  {:unknown, 'b'}
[14474]  {:unknown, 'a'}
[14475]  {:unknown, 'c'}
[14476]  {:unknown, 'k'}
[14477]  {:unknown, ' '}
[14478]  {:unknown, 'o'}
[14479]  {:unknown, 'f'}
[14480]  {:unknown, ' '}
[14481]  {:unknown, 't'}
[14482]  {:unknown, 'h'}
[14483]  {:unknown, 'i'}
[14484]  {:unknown, 's'}
[14485]  {:unknown, ' '}
[14486]  {:unknown, 'a'}
[14487]  {:unknown, 'l'}
[14488]  {:unknown, 'c'}
[14489]  {:unknown, 'o'}
[14490]  {:unknown, 'v'}
[14491]  {:unknown, 'e'}
[14492]  {:unknown, ','}
[14493]  {:unknown, ' '}
[14494]  {:unknown, 't'}
[14495]  {:unknown, 'h'}
[14496]  {:unknown, 'e'}
[14497]  {:unknown, 'r'}
[14498]  {:unknown, 'e'}
[14499]  {:unknown, ' '}
[14500]  {:unknown, 'i'}
[14501]  {:unknown, 's'}
[14502]  {:unknown, ' '}
[14503]  {:unknown, 'a'}
[14504]  {:unknown, ' '}
[14505]  {:unknown, 's'}
[14506]  {:unknown, 'm'}
[14507]  {:unknown, 'a'}
[14508]  {:unknown, 'l'}
[14509]  {:unknown, 'l'}
[14510]  {:unknown, ' '}
[14511]  {:unknown, 't'}
[14512]  {:unknown, 'a'}
[14513]  {:unknown, 'b'}
[14514]  {:unknown, 'l'}
[14515]  {:unknown, 'e'}
[14516]  {:unknown, ','}
[14517]  {:unknown, ' '}
[14518]  {:unknown, 'a'}
[14519]  {:unknown, ' '}
[14520]  {:unknown, 'c'}
[14521]  {:unknown, 'h'}
[14522]  {:unknown, 'a'}
[14523]  {:unknown, 'i'}
[14524]  {:unknown, 'r'}
[14525]  {:unknown, ','}
[14526]  {:unknown, ' '}
[14527]  {:unknown, 'a'}
[14528]  {:unknown, 'n'}
[14529]  {:unknown, 'd'}
[14530]  {:unknown, ' '}
[14531]  {:unknown, 'a'}
[14532]  {:unknown, ' '}
[14533]  {:unknown, 'b'}
[14534]  {:unknown, 'r'}
[14535]  {:unknown, 'o'}
[14536]  {:unknown, 'k'}
[14537]  {:unknown, 'e'}
[14538]  {:unknown, 'n'}
[14539]  {:unknown, ' '}
[14540]  {:unknown, 'l'}
[14541]  {:unknown, 'a'}
[14542]  {:unknown, 'n'}
[14543]  {:unknown, 't'}
[14544]  {:unknown, 'e'}
[14545]  {:unknown, 'r'}
[14546]  {:unknown, 'n'}
[14547]  {:unknown, '.'}
[14548]  {:unknown, ' '}
[14549]  {:unknown, ' '}
[14550]  {:unknown, 'I'}
[14551]  {:unknown, 't'}
[14552]  {:unknown, ' '}
[14553]  {:unknown, 'l'}
[14554]  {:unknown, 'o'}
[14555]  {:unknown, 'o'}
[14556]  {:unknown, 'k'}
[14557]  {:unknown, 's'}
[14558]  {:unknown, ' '}
[14559]  {:unknown, 'l'}
[14560]  {:unknown, 'i'}
[14561]  {:unknown, 'k'}
[14562]  {:unknown, 'e'}
[14563]  {:unknown, ' '}
[14564]  {:unknown, 't'}
[14565]  {:unknown, 'h'}
[14566]  {:unknown, 'i'}
[14567]  {:unknown, 's'}
[14568]  {:unknown, ' '}
[14569]  {:unknown, 's'}
[14570]  {:unknown, 'p'}
[14571]  {:unknown, 'a'}
[14572]  {:unknown, 'c'}
[14573]  {:unknown, 'e'}
[14574]  {:unknown, ' '}
[14575]  {:unknown, 'w'}
[14576]  {:unknown, 'a'}
[14577]  {:unknown, 's'}
[14578]  {:unknown, ' '}
[14579]  {:unknown, 'u'}
[14580]  {:unknown, 's'}
[14581]  {:unknown, 'e'}
[14582]  {:unknown, 'd'}
[14583]  {:unknown, ' '}
[14584]  {:unknown, 'm'}
[14585]  {:unknown, 'u'}
[14586]  {:unknown, 'c'}
[14587]  {:unknown, 'h'}
[14588]  {:unknown, ' '}
[14589]  {:unknown, 'm'}
[14590]  {:unknown, 'o'}
[14591]  {:unknown, 'r'}
[14592]  {:unknown, 'e'}
[14593]  {:unknown, ' '}
[14594]  {:unknown, 'r'}
[14595]  {:unknown, 'e'}
[14596]  {:unknown, 'c'}
[14597]  {:unknown, 'e'}
[14598]  {:unknown, 'n'}
[14599]  {:unknown, 't'}
[14600]  {:unknown, 'l'}
[14601]  {:unknown, 'y'}
[14602]  {:unknown, ' '}
[14603]  {:unknown, 't'}
[14604]  {:unknown, 'h'}
[14605]  {:unknown, 'a'}
[14606]  {:unknown, 'n'}
[14607]  {:unknown, ' '}
[14608]  {:unknown, 't'}
[14609]  {:unknown, 'h'}
[14610]  {:unknown, 'e'}
[14611]  {:unknown, ' '}
[14612]  {:unknown, 'c'}
[14613]  {:unknown, 'a'}
[14614]  {:unknown, 'm'}
[14615]  {:unknown, 'p'}
[14616]  {:unknown, ' '}
[14617]  {:unknown, 't'}
[14618]  {:unknown, 'o'}
[14619]  {:unknown, ' '}
[14620]  {:unknown, 't'}
[14621]  {:unknown, 'h'}
[14622]  {:unknown, 'e'}
[14623]  {:unknown, ' '}
[14624]  {:unknown, 'w'}
[14625]  {:unknown, 'e'}
[14626]  {:unknown, 's'}
[14627]  {:unknown, 't'}
[14628]  {:unknown, '.'}
[14629]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[14633]  {:unknown, 't'}
[14634]  {:or, {:value, 84}, {:value, 114}, {:value, 111}}
[14638]  {:unknown, 'p'}
[14639]  {:unknown, 'i'}
[14640]  {:unknown, 'c'}
[14641]  {:unknown, 'a'}
[14642]  {:unknown, 'l'}
[14643]  {:unknown, ' '}
[14644]  {:unknown, 'C'}
[14645]  {:unknown, 'a'}
[14646]  {:unknown, 'v'}
[14647]  {:unknown, 'e'}
[14648]  {:unknown, 'h'}
[14649]  {:unknown, 'T'}
[14650]  {:unknown, 'h'}
[14651]  {:unknown, 'i'}
[14652]  {:unknown, 's'}
[14653]  {:unknown, ' '}
[14654]  {:unknown, 't'}
[14655]  {:unknown, 'u'}
[14656]  {:unknown, 'n'}
[14657]  {:unknown, 'n'}
[14658]  {:unknown, 'e'}
[14659]  {:unknown, 'l'}
[14660]  {:unknown, ' '}
[14661]  {:unknown, 's'}
[14662]  {:unknown, 'l'}
[14663]  {:unknown, 'o'}
[14664]  {:unknown, 'p'}
[14665]  {:unknown, 'e'}
[14666]  {:unknown, 's'}
[14667]  {:unknown, ' '}
[14668]  {:unknown, 'd'}
[14669]  {:unknown, 'e'}
[14670]  {:unknown, 'e'}
[14671]  {:unknown, 'p'}
[14672]  {:unknown, 'e'}
[14673]  {:unknown, 'r'}
[14674]  {:unknown, ' '}
[14675]  {:unknown, 'u'}
[14676]  {:unknown, 'n'}
[14677]  {:unknown, 'd'}
[14678]  {:unknown, 'e'}
[14679]  {:unknown, 'r'}
[14680]  {:unknown, 'g'}
[14681]  {:unknown, 'r'}
[14682]  {:unknown, 'o'}
[14683]  {:unknown, 'u'}
[14684]  {:unknown, 'n'}
[14685]  {:unknown, 'd'}
[14686]  {:unknown, ' '}
[14687]  {:unknown, 't'}
[14688]  {:unknown, 'o'}
[14689]  {:unknown, ' '}
[14690]  {:unknown, 't'}
[14691]  {:unknown, 'h'}
[14692]  {:unknown, 'e'}
[14693]  {:unknown, ' '}
[14694]  {:unknown, 'n'}
[14695]  {:unknown, 'o'}
[14696]  {:unknown, 'r'}
[14697]  {:unknown, 't'}
[14698]  {:unknown, 'h'}
[14699]  {:unknown, ','}
[14700]  {:unknown, ' '}
[14701]  {:unknown, 'b'}
[14702]  {:unknown, 'u'}
[14703]  {:unknown, 't'}
[14704]  {:unknown, ' '}
[14705]  {:unknown, 't'}
[14706]  {:unknown, 'h'}
[14707]  {:unknown, 'e'}
[14708]  {:unknown, ' '}
[14709]  {:unknown, 'f'}
[14710]  {:unknown, 'i'}
[14711]  {:unknown, 'r'}
[14712]  {:unknown, 'e'}
[14713]  {:unknown, 'f'}
[14714]  {:unknown, 'l'}
[14715]  {:unknown, 'i'}
[14716]  {:unknown, 'e'}
[14717]  {:unknown, 's'}
[14718]  {:unknown, ' '}
[14719]  {:unknown, 'a'}
[14720]  {:unknown, 'r'}
[14721]  {:unknown, 'e'}
[14722]  {:unknown, ' '}
[14723]  {:unknown, 'a'}
[14724]  {:unknown, 'l'}
[14725]  {:unknown, 'l'}
[14726]  {:unknown, ' '}
[14727]  {:unknown, 'a'}
[14728]  {:unknown, 'r'}
[14729]  {:unknown, 'o'}
[14730]  {:unknown, 'u'}
[14731]  {:unknown, 'n'}
[14732]  {:unknown, 'd'}
[14733]  {:unknown, ' '}
[14734]  {:unknown, 't'}
[14735]  {:unknown, 'o'}
[14736]  {:unknown, ' '}
[14737]  {:unknown, 'l'}
[14738]  {:unknown, 'i'}
[14739]  {:unknown, 'g'}
[14740]  {:unknown, 'h'}
[14741]  {:unknown, 't'}
[14742]  {:unknown, ' '}
[14743]  {:unknown, 'y'}
[14744]  {:unknown, 'o'}
[14745]  {:unknown, 'u'}
[14746]  {:unknown, 'r'}
[14747]  {:unknown, ' '}
[14748]  {:unknown, 'p'}
[14749]  {:unknown, 'a'}
[14750]  {:unknown, 't'}
[14751]  {:unknown, 'h'}
[14752]  {:unknown, '.'}
[14753]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[14757]  {:unknown, 't'}
[14758]  {:unknown, 'h'}
[14759]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[14763]  {:unknown, 't'}
[14764]  {:unknown, 'h'}
[14765]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[14769]  {:unknown, 'l'}
[14770]  {:unknown, 't'}
[14771]  {:unknown, ' '}
[14772]  {:unknown, 'L'}
[14773]  {:unknown, 'o'}
[14774]  {:unknown, 'c'}
[14775]  {:unknown, 'k'}
[14776]  {:unknown, [128]}
[14777]  {:unknown, 'Y'}
[14778]  {:unknown, 'o'}
[14779]  {:unknown, 'u'}
[14780]  {:unknown, ' '}
[14781]  {:unknown, 'a'}
[14782]  {:unknown, 'r'}
[14783]  {:unknown, 'e'}
[14784]  {:unknown, ' '}
[14785]  {:unknown, 'i'}
[14786]  {:unknown, 'n'}
[14787]  {:unknown, ' '}
[14788]  {:unknown, 'a'}
[14789]  {:unknown, ' '}
[14790]  {:unknown, 'g'}
[14791]  {:unknown, 'r'}
[14792]  {:unknown, 'i'}
[14793]  {:unknown, 'd'}
[14794]  {:unknown, ' '}
[14795]  {:unknown, 'o'}
[14796]  {:unknown, 'f'}
[14797]  {:unknown, ' '}
[14798]  {:unknown, 'r'}
[14799]  {:unknown, 'o'}
[14800]  {:unknown, 'o'}
[14801]  {:unknown, 'm'}
[14802]  {:unknown, 's'}
[14803]  {:unknown, ' '}
[14804]  {:unknown, 't'}
[14805]  {:unknown, 'h'}
[14806]  {:unknown, 'a'}
[14807]  {:unknown, 't'}
[14808]  {:unknown, ' '}
[14809]  {:unknown, 'c'}
[14810]  {:unknown, 'o'}
[14811]  {:unknown, 'n'}
[14812]  {:unknown, 't'}
[14813]  {:unknown, 'r'}
[14814]  {:unknown, 'o'}
[14815]  {:unknown, 'l'}
[14816]  {:unknown, ' '}
[14817]  {:unknown, 't'}
[14818]  {:unknown, 'h'}
[14819]  {:unknown, 'e'}
[14820]  {:unknown, ' '}
[14821]  {:unknown, 'd'}
[14822]  {:unknown, 'o'}
[14823]  {:unknown, 'o'}
[14824]  {:unknown, 'r'}
[14825]  {:unknown, ' '}
[14826]  {:unknown, 't'}
[14827]  {:unknown, 'o'}
[14828]  {:unknown, ' '}
[14829]  {:unknown, 't'}
[14830]  {:unknown, 'h'}
[14831]  {:unknown, 'e'}
[14832]  {:unknown, ' '}
[14833]  {:unknown, 'v'}
[14834]  {:unknown, 'a'}
[14835]  {:unknown, 'u'}
[14836]  {:unknown, 'l'}
[14837]  {:unknown, 't'}
[14838]  {:unknown, '.'}
[14839]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[14843]  {:unknown, 'e'}
[14844]  {:unknown, ' '}
[14845]  {:unknown, 'f'}
[14846]  {:unknown, 'l'}
[14847]  {:unknown, 'o'}
[14848]  {:unknown, 'o'}
[14849]  {:unknown, 'r'}
[14850]  {:unknown, ' '}
[14851]  {:unknown, 'o'}
[14852]  {:unknown, 'f'}
[14853]  {:unknown, ' '}
[14854]  {:unknown, 't'}
[14855]  {:unknown, 'h'}
[14856]  {:unknown, 'i'}
[14857]  {:unknown, 's'}
[14858]  {:unknown, ' '}
[14859]  {:unknown, 'r'}
[14860]  {:unknown, 'o'}
[14861]  {:unknown, 'o'}
[14862]  {:unknown, 'm'}
[14863]  {:unknown, ' '}
[14864]  {:unknown, 'i'}
[14865]  {:unknown, 's'}
[14866]  {:unknown, ' '}
[14867]  {:unknown, 'a'}
[14868]  {:unknown, ' '}
[14869]  {:unknown, 'l'}
[14870]  {:unknown, 'a'}
[14871]  {:unknown, 'r'}
[14872]  {:unknown, 'g'}
[14873]  {:unknown, 'e'}
[14874]  {:unknown, ' '}
[14875]  {:unknown, 'm'}
[14876]  {:unknown, 'o'}
[14877]  {:unknown, 's'}
[14878]  {:unknown, 'a'}
[14879]  {:unknown, 'i'}
[14880]  {:unknown, 'c'}
[14881]  {:unknown, ' '}
[14882]  {:unknown, 'd'}
[14883]  {:unknown, 'e'}
[14884]  {:unknown, 'p'}
[14885]  {:unknown, 'i'}
[14886]  {:unknown, 'c'}
[14887]  {:unknown, 't'}
[14888]  {:unknown, 'i'}
[14889]  {:unknown, 'n'}
[14890]  {:unknown, 'g'}
[14891]  {:unknown, ' '}
[14892]  {:unknown, 'a'}
[14893]  {:unknown, ' '}
[14894]  {:unknown, '\''}
[14895]  {:unknown, '*'}
[14896]  {:unknown, '\''}
[14897]  {:unknown, ' '}
[14898]  {:unknown, 's'}
[14899]  {:unknown, 'y'}
[14900]  {:unknown, 'm'}
[14901]  {:unknown, 'b'}
[14902]  {:unknown, 'o'}
[14903]  {:unknown, 'l'}
[14904]  {:unknown, '.'}
[14905]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[14909]  {:unknown, 't'}
[14910]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[14914]  {:unknown, 't'}
[14915]  {:unknown, 'h'}
[14916]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[14920]  {:unknown, 'l'}
[14921]  {:unknown, 't'}
[14922]  {:unknown, ' '}
[14923]  {:unknown, 'L'}
[14924]  {:unknown, 'o'}
[14925]  {:unknown, 'c'}
[14926]  {:unknown, 'k'}
[14927]  {:unknown, [130]}
[14928]  {:unknown, 'Y'}
[14929]  {:unknown, 'o'}
[14930]  {:unknown, 'u'}
[14931]  {:unknown, ' '}
[14932]  {:unknown, 'a'}
[14933]  {:unknown, 'r'}
[14934]  {:unknown, 'e'}
[14935]  {:unknown, ' '}
[14936]  {:unknown, 'i'}
[14937]  {:unknown, 'n'}
[14938]  {:unknown, ' '}
[14939]  {:unknown, 'a'}
[14940]  {:unknown, ' '}
[14941]  {:unknown, 'g'}
[14942]  {:unknown, 'r'}
[14943]  {:unknown, 'i'}
[14944]  {:unknown, 'd'}
[14945]  {:unknown, ' '}
[14946]  {:unknown, 'o'}
[14947]  {:unknown, 'f'}
[14948]  {:unknown, ' '}
[14949]  {:unknown, 'r'}
[14950]  {:unknown, 'o'}
[14951]  {:unknown, 'o'}
[14952]  {:unknown, 'm'}
[14953]  {:unknown, 's'}
[14954]  {:unknown, ' '}
[14955]  {:unknown, 't'}
[14956]  {:unknown, 'h'}
[14957]  {:unknown, 'a'}
[14958]  {:unknown, 't'}
[14959]  {:unknown, ' '}
[14960]  {:unknown, 'c'}
[14961]  {:unknown, 'o'}
[14962]  {:unknown, 'n'}
[14963]  {:unknown, 't'}
[14964]  {:unknown, 'r'}
[14965]  {:unknown, 'o'}
[14966]  {:unknown, 'l'}
[14967]  {:unknown, ' '}
[14968]  {:unknown, 't'}
[14969]  {:unknown, 'h'}
[14970]  {:unknown, 'e'}
[14971]  {:unknown, ' '}
[14972]  {:unknown, 'd'}
[14973]  {:unknown, 'o'}
[14974]  {:unknown, 'o'}
[14975]  {:unknown, 'r'}
[14976]  {:unknown, ' '}
[14977]  {:unknown, 't'}
[14978]  {:unknown, 'o'}
[14979]  {:unknown, ' '}
[14980]  {:unknown, 't'}
[14981]  {:unknown, 'h'}
[14982]  {:unknown, 'e'}
[14983]  {:unknown, ' '}
[14984]  {:unknown, 'v'}
[14985]  {:unknown, 'a'}
[14986]  {:unknown, 'u'}
[14987]  {:unknown, 'l'}
[14988]  {:unknown, 't'}
[14989]  {:unknown, '.'}
[14990]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[14994]  {:unknown, 'e'}
[14995]  {:unknown, ' '}
[14996]  {:unknown, 'f'}
[14997]  {:unknown, 'l'}
[14998]  {:unknown, 'o'}
[14999]  {:unknown, 'o'}
[15000]  {:unknown, 'r'}
[15001]  {:unknown, ' '}
[15002]  {:unknown, 'o'}
[15003]  {:unknown, 'f'}
[15004]  {:unknown, ' '}
[15005]  {:unknown, 't'}
[15006]  {:unknown, 'h'}
[15007]  {:unknown, 'i'}
[15008]  {:unknown, 's'}
[15009]  {:unknown, ' '}
[15010]  {:unknown, 'r'}
[15011]  {:unknown, 'o'}
[15012]  {:unknown, 'o'}
[15013]  {:unknown, 'm'}
[15014]  {:unknown, ' '}
[15015]  {:unknown, 'i'}
[15016]  {:unknown, 's'}
[15017]  {:unknown, ' '}
[15018]  {:unknown, 'a'}
[15019]  {:unknown, ' '}
[15020]  {:unknown, 'l'}
[15021]  {:unknown, 'a'}
[15022]  {:unknown, 'r'}
[15023]  {:unknown, 'g'}
[15024]  {:unknown, 'e'}
[15025]  {:unknown, ' '}
[15026]  {:unknown, 'm'}
[15027]  {:unknown, 'o'}
[15028]  {:unknown, 's'}
[15029]  {:unknown, 'a'}
[15030]  {:unknown, 'i'}
[15031]  {:unknown, 'c'}
[15032]  {:unknown, ' '}
[15033]  {:unknown, 'd'}
[15034]  {:unknown, 'e'}
[15035]  {:unknown, 'p'}
[15036]  {:unknown, 'i'}
[15037]  {:unknown, 'c'}
[15038]  {:unknown, 't'}
[15039]  {:unknown, 'i'}
[15040]  {:unknown, 'n'}
[15041]  {:unknown, 'g'}
[15042]  {:unknown, ' '}
[15043]  {:unknown, 't'}
[15044]  {:unknown, 'h'}
[15045]  {:unknown, 'e'}
[15046]  {:unknown, ' '}
[15047]  {:unknown, 'n'}
[15048]  {:unknown, 'u'}
[15049]  {:unknown, 'm'}
[15050]  {:unknown, 'b'}
[15051]  {:unknown, 'e'}
[15052]  {:unknown, 'r'}
[15053]  {:unknown, ' '}
[15054]  {:unknown, '\''}
[15055]  {:unknown, '8'}
[15056]  {:unknown, '\''}
[15057]  {:unknown, '.'}
[15058]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[15062]  {:unknown, 't'}
[15063]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15067]  {:unknown, 't'}
[15068]  {:unknown, 'h'}
[15069]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[15073]  {:unknown, 't'}
[15074]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15078]  {:unknown, 'l'}
[15079]  {:unknown, 't'}
[15080]  {:unknown, ' '}
[15081]  {:unknown, 'L'}
[15082]  {:unknown, 'o'}
[15083]  {:unknown, 'c'}
[15084]  {:unknown, 'k'}
[15085]  {:unknown, [128]}
[15086]  {:unknown, 'Y'}
[15087]  {:unknown, 'o'}
[15088]  {:unknown, 'u'}
[15089]  {:unknown, ' '}
[15090]  {:unknown, 'a'}
[15091]  {:unknown, 'r'}
[15092]  {:unknown, 'e'}
[15093]  {:unknown, ' '}
[15094]  {:unknown, 'i'}
[15095]  {:unknown, 'n'}
[15096]  {:unknown, ' '}
[15097]  {:unknown, 'a'}
[15098]  {:unknown, ' '}
[15099]  {:unknown, 'g'}
[15100]  {:unknown, 'r'}
[15101]  {:unknown, 'i'}
[15102]  {:unknown, 'd'}
[15103]  {:unknown, ' '}
[15104]  {:unknown, 'o'}
[15105]  {:unknown, 'f'}
[15106]  {:unknown, ' '}
[15107]  {:unknown, 'r'}
[15108]  {:unknown, 'o'}
[15109]  {:unknown, 'o'}
[15110]  {:unknown, 'm'}
[15111]  {:unknown, 's'}
[15112]  {:unknown, ' '}
[15113]  {:unknown, 't'}
[15114]  {:unknown, 'h'}
[15115]  {:unknown, 'a'}
[15116]  {:unknown, 't'}
[15117]  {:unknown, ' '}
[15118]  {:unknown, 'c'}
[15119]  {:unknown, 'o'}
[15120]  {:unknown, 'n'}
[15121]  {:unknown, 't'}
[15122]  {:unknown, 'r'}
[15123]  {:unknown, 'o'}
[15124]  {:unknown, 'l'}
[15125]  {:unknown, ' '}
[15126]  {:unknown, 't'}
[15127]  {:unknown, 'h'}
[15128]  {:unknown, 'e'}
[15129]  {:unknown, ' '}
[15130]  {:unknown, 'd'}
[15131]  {:unknown, 'o'}
[15132]  {:unknown, 'o'}
[15133]  {:unknown, 'r'}
[15134]  {:unknown, ' '}
[15135]  {:unknown, 't'}
[15136]  {:unknown, 'o'}
[15137]  {:unknown, ' '}
[15138]  {:unknown, 't'}
[15139]  {:unknown, 'h'}
[15140]  {:unknown, 'e'}
[15141]  {:unknown, ' '}
[15142]  {:unknown, 'v'}
[15143]  {:unknown, 'a'}
[15144]  {:unknown, 'u'}
[15145]  {:unknown, 'l'}
[15146]  {:unknown, 't'}
[15147]  {:unknown, '.'}
[15148]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[15152]  {:unknown, 'e'}
[15153]  {:unknown, ' '}
[15154]  {:unknown, 'f'}
[15155]  {:unknown, 'l'}
[15156]  {:unknown, 'o'}
[15157]  {:unknown, 'o'}
[15158]  {:unknown, 'r'}
[15159]  {:unknown, ' '}
[15160]  {:unknown, 'o'}
[15161]  {:unknown, 'f'}
[15162]  {:unknown, ' '}
[15163]  {:unknown, 't'}
[15164]  {:unknown, 'h'}
[15165]  {:unknown, 'i'}
[15166]  {:unknown, 's'}
[15167]  {:unknown, ' '}
[15168]  {:unknown, 'r'}
[15169]  {:unknown, 'o'}
[15170]  {:unknown, 'o'}
[15171]  {:unknown, 'm'}
[15172]  {:unknown, ' '}
[15173]  {:unknown, 'i'}
[15174]  {:unknown, 's'}
[15175]  {:unknown, ' '}
[15176]  {:unknown, 'a'}
[15177]  {:unknown, ' '}
[15178]  {:unknown, 'l'}
[15179]  {:unknown, 'a'}
[15180]  {:unknown, 'r'}
[15181]  {:unknown, 'g'}
[15182]  {:unknown, 'e'}
[15183]  {:unknown, ' '}
[15184]  {:unknown, 'm'}
[15185]  {:unknown, 'o'}
[15186]  {:unknown, 's'}
[15187]  {:unknown, 'a'}
[15188]  {:unknown, 'i'}
[15189]  {:unknown, 'c'}
[15190]  {:unknown, ' '}
[15191]  {:unknown, 'd'}
[15192]  {:unknown, 'e'}
[15193]  {:unknown, 'p'}
[15194]  {:unknown, 'i'}
[15195]  {:unknown, 'c'}
[15196]  {:unknown, 't'}
[15197]  {:unknown, 'i'}
[15198]  {:unknown, 'n'}
[15199]  {:unknown, 'g'}
[15200]  {:unknown, ' '}
[15201]  {:unknown, 'a'}
[15202]  {:unknown, ' '}
[15203]  {:unknown, '\''}
[15204]  {:unknown, '-'}
[15205]  {:unknown, '\''}
[15206]  {:unknown, ' '}
[15207]  {:unknown, 's'}
[15208]  {:unknown, 'y'}
[15209]  {:unknown, 'm'}
[15210]  {:unknown, 'b'}
[15211]  {:unknown, 'o'}
[15212]  {:unknown, 'l'}
[15213]  {:unknown, '.'}
[15214]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[15218]  {:unknown, 't'}
[15219]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15223]  {:unknown, 't'}
[15224]  {:unknown, 'h'}
[15225]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[15229]  {:unknown, 't'}
[15230]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15234]  {:unknown, 'l'}
[15235]  {:unknown, 't'}
[15236]  {:unknown, ' '}
[15237]  {:unknown, 'D'}
[15238]  {:unknown, 'o'}
[15239]  {:unknown, 'o'}
[15240]  {:unknown, 'r'}
[15241]  {:unknown, [246]}
[15242]  {:unknown, 'Y'}
[15243]  {:unknown, 'o'}
[15244]  {:unknown, 'u'}
[15245]  {:unknown, ' '}
[15246]  {:unknown, 's'}
[15247]  {:unknown, 't'}
[15248]  {:unknown, 'a'}
[15249]  {:unknown, 'n'}
[15250]  {:unknown, 'd'}
[15251]  {:unknown, ' '}
[15252]  {:unknown, 'b'}
[15253]  {:unknown, 'e'}
[15254]  {:unknown, 'f'}
[15255]  {:unknown, 'o'}
[15256]  {:unknown, 'r'}
[15257]  {:unknown, 'e'}
[15258]  {:unknown, ' '}
[15259]  {:unknown, 't'}
[15260]  {:unknown, 'h'}
[15261]  {:unknown, 'e'}
[15262]  {:unknown, ' '}
[15263]  {:unknown, 'd'}
[15264]  {:unknown, 'o'}
[15265]  {:unknown, 'o'}
[15266]  {:unknown, 'r'}
[15267]  {:unknown, ' '}
[15268]  {:unknown, 't'}
[15269]  {:unknown, 'o'}
[15270]  {:unknown, ' '}
[15271]  {:unknown, 't'}
[15272]  {:unknown, 'h'}
[15273]  {:unknown, 'e'}
[15274]  {:unknown, ' '}
[15275]  {:unknown, 'v'}
[15276]  {:unknown, 'a'}
[15277]  {:unknown, 'u'}
[15278]  {:unknown, 'l'}
[15279]  {:unknown, 't'}
[15280]  {:unknown, ';'}
[15281]  {:unknown, ' '}
[15282]  {:unknown, 'i'}
[15283]  {:unknown, 't'}
[15284]  {:unknown, ' '}
[15285]  {:unknown, 'h'}
[15286]  {:unknown, 'a'}
[15287]  {:unknown, 's'}
[15288]  {:unknown, ' '}
[15289]  {:unknown, 'a'}
[15290]  {:unknown, ' '}
[15291]  {:unknown, 'l'}
[15292]  {:unknown, 'a'}
[15293]  {:unknown, 'r'}
[15294]  {:unknown, 'g'}
[15295]  {:unknown, 'e'}
[15296]  {:unknown, ' '}
[15297]  {:unknown, '\''}
[15298]  {:unknown, '3'}
[15299]  {:unknown, '0'}
[15300]  {:unknown, '\''}
[15301]  {:unknown, ' '}
[15302]  {:unknown, 'c'}
[15303]  {:unknown, 'a'}
[15304]  {:unknown, 'r'}
[15305]  {:unknown, 'v'}
[15306]  {:unknown, 'e'}
[15307]  {:unknown, 'd'}
[15308]  {:unknown, ' '}
[15309]  {:unknown, 'i'}
[15310]  {:unknown, 'n'}
[15311]  {:unknown, 't'}
[15312]  {:unknown, 'o'}
[15313]  {:unknown, ' '}
[15314]  {:unknown, 'i'}
[15315]  {:unknown, 't'}
[15316]  {:unknown, '.'}
[15317]  {:unknown, ' '}
[15318]  {:unknown, ' '}
[15319]  {:unknown, 'A'}
[15320]  {:unknown, 'f'}
[15321]  {:unknown, 'f'}
[15322]  {:unknown, 'i'}
[15323]  {:unknown, 'x'}
[15324]  {:unknown, 'e'}
[15325]  {:unknown, 'd'}
[15326]  {:unknown, ' '}
[15327]  {:unknown, 't'}
[15328]  {:unknown, 'o'}
[15329]  {:unknown, ' '}
[15330]  {:unknown, 't'}
[15331]  {:unknown, 'h'}
[15332]  {:unknown, 'e'}
[15333]  {:unknown, ' '}
[15334]  {:unknown, 'w'}
[15335]  {:unknown, 'a'}
[15336]  {:unknown, 'l'}
[15337]  {:unknown, 'l'}
[15338]  {:unknown, ' '}
[15339]  {:unknown, 'n'}
[15340]  {:unknown, 'e'}
[15341]  {:unknown, 'a'}
[15342]  {:unknown, 'r'}
[15343]  {:unknown, ' '}
[15344]  {:unknown, 't'}
[15345]  {:unknown, 'h'}
[15346]  {:unknown, 'e'}
[15347]  {:unknown, ' '}
[15348]  {:unknown, 'd'}
[15349]  {:unknown, 'o'}
[15350]  {:unknown, 'o'}
[15351]  {:unknown, 'r'}
[15352]  {:unknown, ','}
[15353]  {:unknown, ' '}
[15354]  {:unknown, 't'}
[15355]  {:unknown, 'h'}
[15356]  {:unknown, 'e'}
[15357]  {:unknown, 'r'}
[15358]  {:unknown, 'e'}
[15359]  {:unknown, ' '}
[15360]  {:unknown, 'i'}
[15361]  {:unknown, 's'}
[15362]  {:unknown, ' '}
[15363]  {:unknown, 'a'}
[15364]  {:unknown, ' '}
[15365]  {:unknown, 'r'}
[15366]  {:unknown, 'u'}
[15367]  {:unknown, 'n'}
[15368]  {:unknown, 'n'}
[15369]  {:unknown, 'i'}
[15370]  {:unknown, 'n'}
[15371]  {:unknown, 'g'}
[15372]  {:unknown, ' '}
[15373]  {:unknown, 'h'}
[15374]  {:unknown, 'o'}
[15375]  {:unknown, 'u'}
[15376]  {:unknown, 'r'}
[15377]  {:unknown, 'g'}
[15378]  {:unknown, 'l'}
[15379]  {:unknown, 'a'}
[15380]  {:unknown, 's'}
[15381]  {:unknown, 's'}
[15382]  {:unknown, ' '}
[15383]  {:unknown, 'w'}
[15384]  {:unknown, 'h'}
[15385]  {:unknown, 'i'}
[15386]  {:unknown, 'c'}
[15387]  {:unknown, 'h'}
[15388]  {:unknown, ' '}
[15389]  {:unknown, 'n'}
[15390]  {:unknown, 'e'}
[15391]  {:unknown, 'v'}
[15392]  {:unknown, 'e'}
[15393]  {:unknown, 'r'}
[15394]  {:unknown, ' '}
[15395]  {:unknown, 's'}
[15396]  {:unknown, 'e'}
[15397]  {:unknown, 'e'}
[15398]  {:unknown, 'm'}
[15399]  {:unknown, 's'}
[15400]  {:unknown, ' '}
[15401]  {:unknown, 't'}
[15402]  {:unknown, 'o'}
[15403]  {:unknown, ' '}
[15404]  {:unknown, 'r'}
[15405]  {:unknown, 'u'}
[15406]  {:unknown, 'n'}
[15407]  {:unknown, ' '}
[15408]  {:unknown, 'o'}
[15409]  {:unknown, 'u'}
[15410]  {:unknown, 't'}
[15411]  {:unknown, ' '}
[15412]  {:unknown, 'o'}
[15413]  {:unknown, 'f'}
[15414]  {:unknown, ' '}
[15415]  {:unknown, 's'}
[15416]  {:unknown, 'a'}
[15417]  {:unknown, 'n'}
[15418]  {:unknown, 'd'}
[15419]  {:unknown, '.'}
[15420]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[15424]  {:unknown, 'e'}
[15425]  {:unknown, ' '}
[15426]  {:unknown, 'f'}
[15427]  {:unknown, 'l'}
[15428]  {:unknown, 'o'}
[15429]  {:unknown, 'o'}
[15430]  {:unknown, 'r'}
[15431]  {:unknown, ' '}
[15432]  {:unknown, 'o'}
[15433]  {:unknown, 'f'}
[15434]  {:unknown, ' '}
[15435]  {:unknown, 't'}
[15436]  {:unknown, 'h'}
[15437]  {:unknown, 'i'}
[15438]  {:unknown, 's'}
[15439]  {:unknown, ' '}
[15440]  {:unknown, 'r'}
[15441]  {:unknown, 'o'}
[15442]  {:unknown, 'o'}
[15443]  {:unknown, 'm'}
[15444]  {:unknown, ' '}
[15445]  {:unknown, 'i'}
[15446]  {:unknown, 's'}
[15447]  {:unknown, ' '}
[15448]  {:unknown, 'a'}
[15449]  {:unknown, ' '}
[15450]  {:unknown, 'l'}
[15451]  {:unknown, 'a'}
[15452]  {:unknown, 'r'}
[15453]  {:unknown, 'g'}
[15454]  {:unknown, 'e'}
[15455]  {:unknown, ' '}
[15456]  {:unknown, 'm'}
[15457]  {:unknown, 'o'}
[15458]  {:unknown, 's'}
[15459]  {:unknown, 'a'}
[15460]  {:unknown, 'i'}
[15461]  {:unknown, 'c'}
[15462]  {:unknown, ' '}
[15463]  {:unknown, 'd'}
[15464]  {:unknown, 'e'}
[15465]  {:unknown, 'p'}
[15466]  {:unknown, 'i'}
[15467]  {:unknown, 'c'}
[15468]  {:unknown, 't'}
[15469]  {:unknown, 'i'}
[15470]  {:unknown, 'n'}
[15471]  {:unknown, 'g'}
[15472]  {:unknown, ' '}
[15473]  {:unknown, 't'}
[15474]  {:unknown, 'h'}
[15475]  {:unknown, 'e'}
[15476]  {:unknown, ' '}
[15477]  {:unknown, 'n'}
[15478]  {:unknown, 'u'}
[15479]  {:unknown, 'm'}
[15480]  {:unknown, 'b'}
[15481]  {:unknown, 'e'}
[15482]  {:unknown, 'r'}
[15483]  {:unknown, ' '}
[15484]  {:unknown, '\''}
[15485]  {:unknown, '1'}
[15486]  {:unknown, '\''}
[15487]  {:unknown, '.'}
[15488]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15492]  {:unknown, 't'}
[15493]  {:unknown, 'h'}
[15494]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[15498]  {:unknown, 't'}
[15499]  {:gt, {:value, 118}, {:value, 97}, {:value, 117}}
[15503]  {:unknown, 'l'}
[15504]  {:unknown, 't'}
[15505]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15509]  {:unknown, 'l'}
[15510]  {:unknown, 't'}
[15511]  {:unknown, ' '}
[15512]  {:unknown, 'L'}
[15513]  {:unknown, 'o'}
[15514]  {:unknown, 'c'}
[15515]  {:unknown, 'k'}
[15516]  {:unknown, [130]}
[15517]  {:unknown, 'Y'}
[15518]  {:unknown, 'o'}
[15519]  {:unknown, 'u'}
[15520]  {:unknown, ' '}
[15521]  {:unknown, 'a'}
[15522]  {:unknown, 'r'}
[15523]  {:unknown, 'e'}
[15524]  {:unknown, ' '}
[15525]  {:unknown, 'i'}
[15526]  {:unknown, 'n'}
[15527]  {:unknown, ' '}
[15528]  {:unknown, 'a'}
[15529]  {:unknown, ' '}
[15530]  {:unknown, 'g'}
[15531]  {:unknown, 'r'}
[15532]  {:unknown, 'i'}
[15533]  {:unknown, 'd'}
[15534]  {:unknown, ' '}
[15535]  {:unknown, 'o'}
[15536]  {:unknown, 'f'}
[15537]  {:unknown, ' '}
[15538]  {:unknown, 'r'}
[15539]  {:unknown, 'o'}
[15540]  {:unknown, 'o'}
[15541]  {:unknown, 'm'}
[15542]  {:unknown, 's'}
[15543]  {:unknown, ' '}
[15544]  {:unknown, 't'}
[15545]  {:unknown, 'h'}
[15546]  {:unknown, 'a'}
[15547]  {:unknown, 't'}
[15548]  {:unknown, ' '}
[15549]  {:unknown, 'c'}
[15550]  {:unknown, 'o'}
[15551]  {:unknown, 'n'}
[15552]  {:unknown, 't'}
[15553]  {:unknown, 'r'}
[15554]  {:unknown, 'o'}
[15555]  {:unknown, 'l'}
[15556]  {:unknown, ' '}
[15557]  {:unknown, 't'}
[15558]  {:unknown, 'h'}
[15559]  {:unknown, 'e'}
[15560]  {:unknown, ' '}
[15561]  {:unknown, 'd'}
[15562]  {:unknown, 'o'}
[15563]  {:unknown, 'o'}
[15564]  {:unknown, 'r'}
[15565]  {:unknown, ' '}
[15566]  {:unknown, 't'}
[15567]  {:unknown, 'o'}
[15568]  {:unknown, ' '}
[15569]  {:unknown, 't'}
[15570]  {:unknown, 'h'}
[15571]  {:unknown, 'e'}
[15572]  {:unknown, ' '}
[15573]  {:unknown, 'v'}
[15574]  {:unknown, 'a'}
[15575]  {:unknown, 'u'}
[15576]  {:unknown, 'l'}
[15577]  {:unknown, 't'}
[15578]  {:unknown, '.'}
[15579]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[15583]  {:unknown, 'e'}
[15584]  {:unknown, ' '}
[15585]  {:unknown, 'f'}
[15586]  {:unknown, 'l'}
[15587]  {:unknown, 'o'}
[15588]  {:unknown, 'o'}
[15589]  {:unknown, 'r'}
[15590]  {:unknown, ' '}
[15591]  {:unknown, 'o'}
[15592]  {:unknown, 'f'}
[15593]  {:unknown, ' '}
[15594]  {:unknown, 't'}
[15595]  {:unknown, 'h'}
[15596]  {:unknown, 'i'}
[15597]  {:unknown, 's'}
[15598]  {:unknown, ' '}
[15599]  {:unknown, 'r'}
[15600]  {:unknown, 'o'}
[15601]  {:unknown, 'o'}
[15602]  {:unknown, 'm'}
[15603]  {:unknown, ' '}
[15604]  {:unknown, 'i'}
[15605]  {:unknown, 's'}
[15606]  {:unknown, ' '}
[15607]  {:unknown, 'a'}
[15608]  {:unknown, ' '}
[15609]  {:unknown, 'l'}
[15610]  {:unknown, 'a'}
[15611]  {:unknown, 'r'}
[15612]  {:unknown, 'g'}
[15613]  {:unknown, 'e'}
[15614]  {:unknown, ' '}
[15615]  {:unknown, 'm'}
[15616]  {:unknown, 'o'}
[15617]  {:unknown, 's'}
[15618]  {:unknown, 'a'}
[15619]  {:unknown, 'i'}
[15620]  {:unknown, 'c'}
[15621]  {:unknown, ' '}
[15622]  {:unknown, 'd'}
[15623]  {:unknown, 'e'}
[15624]  {:unknown, 'p'}
[15625]  {:unknown, 'i'}
[15626]  {:unknown, 'c'}
[15627]  {:unknown, 't'}
[15628]  {:unknown, 'i'}
[15629]  {:unknown, 'n'}
[15630]  {:unknown, 'g'}
[15631]  {:unknown, ' '}
[15632]  {:unknown, 't'}
[15633]  {:unknown, 'h'}
[15634]  {:unknown, 'e'}
[15635]  {:unknown, ' '}
[15636]  {:unknown, 'n'}
[15637]  {:unknown, 'u'}
[15638]  {:unknown, 'm'}
[15639]  {:unknown, 'b'}
[15640]  {:unknown, 'e'}
[15641]  {:unknown, 'r'}
[15642]  {:unknown, ' '}
[15643]  {:unknown, '\''}
[15644]  {:unknown, '4'}
[15645]  {:unknown, '\''}
[15646]  {:unknown, '.'}
[15647]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[15651]  {:unknown, 't'}
[15652]  {:unknown, 'h'}
[15653]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[15657]  {:unknown, 't'}
[15658]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15662]  {:unknown, 't'}
[15663]  {:unknown, 'h'}
[15664]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15668]  {:unknown, 'l'}
[15669]  {:unknown, 't'}
[15670]  {:unknown, ' '}
[15671]  {:unknown, 'L'}
[15672]  {:unknown, 'o'}
[15673]  {:unknown, 'c'}
[15674]  {:unknown, 'k'}
[15675]  {:unknown, [128]}
[15676]  {:unknown, 'Y'}
[15677]  {:unknown, 'o'}
[15678]  {:unknown, 'u'}
[15679]  {:unknown, ' '}
[15680]  {:unknown, 'a'}
[15681]  {:unknown, 'r'}
[15682]  {:unknown, 'e'}
[15683]  {:unknown, ' '}
[15684]  {:unknown, 'i'}
[15685]  {:unknown, 'n'}
[15686]  {:unknown, ' '}
[15687]  {:unknown, 'a'}
[15688]  {:unknown, ' '}
[15689]  {:unknown, 'g'}
[15690]  {:unknown, 'r'}
[15691]  {:unknown, 'i'}
[15692]  {:unknown, 'd'}
[15693]  {:unknown, ' '}
[15694]  {:unknown, 'o'}
[15695]  {:unknown, 'f'}
[15696]  {:unknown, ' '}
[15697]  {:unknown, 'r'}
[15698]  {:unknown, 'o'}
[15699]  {:unknown, 'o'}
[15700]  {:unknown, 'm'}
[15701]  {:unknown, 's'}
[15702]  {:unknown, ' '}
[15703]  {:unknown, 't'}
[15704]  {:unknown, 'h'}
[15705]  {:unknown, 'a'}
[15706]  {:unknown, 't'}
[15707]  {:unknown, ' '}
[15708]  {:unknown, 'c'}
[15709]  {:unknown, 'o'}
[15710]  {:unknown, 'n'}
[15711]  {:unknown, 't'}
[15712]  {:unknown, 'r'}
[15713]  {:unknown, 'o'}
[15714]  {:unknown, 'l'}
[15715]  {:unknown, ' '}
[15716]  {:unknown, 't'}
[15717]  {:unknown, 'h'}
[15718]  {:unknown, 'e'}
[15719]  {:unknown, ' '}
[15720]  {:unknown, 'd'}
[15721]  {:unknown, 'o'}
[15722]  {:unknown, 'o'}
[15723]  {:unknown, 'r'}
[15724]  {:unknown, ' '}
[15725]  {:unknown, 't'}
[15726]  {:unknown, 'o'}
[15727]  {:unknown, ' '}
[15728]  {:unknown, 't'}
[15729]  {:unknown, 'h'}
[15730]  {:unknown, 'e'}
[15731]  {:unknown, ' '}
[15732]  {:unknown, 'v'}
[15733]  {:unknown, 'a'}
[15734]  {:unknown, 'u'}
[15735]  {:unknown, 'l'}
[15736]  {:unknown, 't'}
[15737]  {:unknown, '.'}
[15738]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[15742]  {:unknown, 'e'}
[15743]  {:unknown, ' '}
[15744]  {:unknown, 'f'}
[15745]  {:unknown, 'l'}
[15746]  {:unknown, 'o'}
[15747]  {:unknown, 'o'}
[15748]  {:unknown, 'r'}
[15749]  {:unknown, ' '}
[15750]  {:unknown, 'o'}
[15751]  {:unknown, 'f'}
[15752]  {:unknown, ' '}
[15753]  {:unknown, 't'}
[15754]  {:unknown, 'h'}
[15755]  {:unknown, 'i'}
[15756]  {:unknown, 's'}
[15757]  {:unknown, ' '}
[15758]  {:unknown, 'r'}
[15759]  {:unknown, 'o'}
[15760]  {:unknown, 'o'}
[15761]  {:unknown, 'm'}
[15762]  {:unknown, ' '}
[15763]  {:unknown, 'i'}
[15764]  {:unknown, 's'}
[15765]  {:unknown, ' '}
[15766]  {:unknown, 'a'}
[15767]  {:unknown, ' '}
[15768]  {:unknown, 'l'}
[15769]  {:unknown, 'a'}
[15770]  {:unknown, 'r'}
[15771]  {:unknown, 'g'}
[15772]  {:unknown, 'e'}
[15773]  {:unknown, ' '}
[15774]  {:unknown, 'm'}
[15775]  {:unknown, 'o'}
[15776]  {:unknown, 's'}
[15777]  {:unknown, 'a'}
[15778]  {:unknown, 'i'}
[15779]  {:unknown, 'c'}
[15780]  {:unknown, ' '}
[15781]  {:unknown, 'd'}
[15782]  {:unknown, 'e'}
[15783]  {:unknown, 'p'}
[15784]  {:unknown, 'i'}
[15785]  {:unknown, 'c'}
[15786]  {:unknown, 't'}
[15787]  {:unknown, 'i'}
[15788]  {:unknown, 'n'}
[15789]  {:unknown, 'g'}
[15790]  {:unknown, ' '}
[15791]  {:unknown, 'a'}
[15792]  {:unknown, ' '}
[15793]  {:unknown, '\''}
[15794]  {:unknown, '*'}
[15795]  {:unknown, '\''}
[15796]  {:unknown, ' '}
[15797]  {:unknown, 's'}
[15798]  {:unknown, 'y'}
[15799]  {:unknown, 'm'}
[15800]  {:unknown, 'b'}
[15801]  {:unknown, 'o'}
[15802]  {:unknown, 'l'}
[15803]  {:unknown, '.'}
[15804]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[15808]  {:unknown, 't'}
[15809]  {:unknown, 'h'}
[15810]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[15814]  {:unknown, 't'}
[15815]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15819]  {:unknown, 't'}
[15820]  {:unknown, 'h'}
[15821]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[15825]  {:unknown, 't'}
[15826]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15830]  {:unknown, 'l'}
[15831]  {:unknown, 't'}
[15832]  {:unknown, ' '}
[15833]  {:unknown, 'L'}
[15834]  {:unknown, 'o'}
[15835]  {:unknown, 'c'}
[15836]  {:unknown, 'k'}
[15837]  {:unknown, [131]}
[15838]  {:unknown, 'Y'}
[15839]  {:unknown, 'o'}
[15840]  {:unknown, 'u'}
[15841]  {:unknown, ' '}
[15842]  {:unknown, 'a'}
[15843]  {:unknown, 'r'}
[15844]  {:unknown, 'e'}
[15845]  {:unknown, ' '}
[15846]  {:unknown, 'i'}
[15847]  {:unknown, 'n'}
[15848]  {:unknown, ' '}
[15849]  {:unknown, 'a'}
[15850]  {:unknown, ' '}
[15851]  {:unknown, 'g'}
[15852]  {:unknown, 'r'}
[15853]  {:unknown, 'i'}
[15854]  {:unknown, 'd'}
[15855]  {:unknown, ' '}
[15856]  {:unknown, 'o'}
[15857]  {:unknown, 'f'}
[15858]  {:unknown, ' '}
[15859]  {:unknown, 'r'}
[15860]  {:unknown, 'o'}
[15861]  {:unknown, 'o'}
[15862]  {:unknown, 'm'}
[15863]  {:unknown, 's'}
[15864]  {:unknown, ' '}
[15865]  {:unknown, 't'}
[15866]  {:unknown, 'h'}
[15867]  {:unknown, 'a'}
[15868]  {:unknown, 't'}
[15869]  {:unknown, ' '}
[15870]  {:unknown, 'c'}
[15871]  {:unknown, 'o'}
[15872]  {:unknown, 'n'}
[15873]  {:unknown, 't'}
[15874]  {:unknown, 'r'}
[15875]  {:unknown, 'o'}
[15876]  {:unknown, 'l'}
[15877]  {:unknown, ' '}
[15878]  {:unknown, 't'}
[15879]  {:unknown, 'h'}
[15880]  {:unknown, 'e'}
[15881]  {:unknown, ' '}
[15882]  {:unknown, 'd'}
[15883]  {:unknown, 'o'}
[15884]  {:unknown, 'o'}
[15885]  {:unknown, 'r'}
[15886]  {:unknown, ' '}
[15887]  {:unknown, 't'}
[15888]  {:unknown, 'o'}
[15889]  {:unknown, ' '}
[15890]  {:unknown, 't'}
[15891]  {:unknown, 'h'}
[15892]  {:unknown, 'e'}
[15893]  {:unknown, ' '}
[15894]  {:unknown, 'v'}
[15895]  {:unknown, 'a'}
[15896]  {:unknown, 'u'}
[15897]  {:unknown, 'l'}
[15898]  {:unknown, 't'}
[15899]  {:unknown, '.'}
[15900]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[15904]  {:unknown, 'e'}
[15905]  {:unknown, ' '}
[15906]  {:unknown, 'f'}
[15907]  {:unknown, 'l'}
[15908]  {:unknown, 'o'}
[15909]  {:unknown, 'o'}
[15910]  {:unknown, 'r'}
[15911]  {:unknown, ' '}
[15912]  {:unknown, 'o'}
[15913]  {:unknown, 'f'}
[15914]  {:unknown, ' '}
[15915]  {:unknown, 't'}
[15916]  {:unknown, 'h'}
[15917]  {:unknown, 'i'}
[15918]  {:unknown, 's'}
[15919]  {:unknown, ' '}
[15920]  {:unknown, 'r'}
[15921]  {:unknown, 'o'}
[15922]  {:unknown, 'o'}
[15923]  {:unknown, 'm'}
[15924]  {:unknown, ' '}
[15925]  {:unknown, 'i'}
[15926]  {:unknown, 's'}
[15927]  {:unknown, ' '}
[15928]  {:unknown, 'a'}
[15929]  {:unknown, ' '}
[15930]  {:unknown, 'l'}
[15931]  {:unknown, 'a'}
[15932]  {:unknown, 'r'}
[15933]  {:unknown, 'g'}
[15934]  {:unknown, 'e'}
[15935]  {:unknown, ' '}
[15936]  {:unknown, 'm'}
[15937]  {:unknown, 'o'}
[15938]  {:unknown, 's'}
[15939]  {:unknown, 'a'}
[15940]  {:unknown, 'i'}
[15941]  {:unknown, 'c'}
[15942]  {:unknown, ' '}
[15943]  {:unknown, 'd'}
[15944]  {:unknown, 'e'}
[15945]  {:unknown, 'p'}
[15946]  {:unknown, 'i'}
[15947]  {:unknown, 'c'}
[15948]  {:unknown, 't'}
[15949]  {:unknown, 'i'}
[15950]  {:unknown, 'n'}
[15951]  {:unknown, 'g'}
[15952]  {:unknown, ' '}
[15953]  {:unknown, 't'}
[15954]  {:unknown, 'h'}
[15955]  {:unknown, 'e'}
[15956]  {:unknown, ' '}
[15957]  {:unknown, 'n'}
[15958]  {:unknown, 'u'}
[15959]  {:unknown, 'm'}
[15960]  {:unknown, 'b'}
[15961]  {:unknown, 'e'}
[15962]  {:unknown, 'r'}
[15963]  {:unknown, ' '}
[15964]  {:unknown, '\''}
[15965]  {:unknown, '1'}
[15966]  {:unknown, '1'}
[15967]  {:unknown, '\''}
[15968]  {:unknown, '.'}
[15969]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[15973]  {:unknown, 't'}
[15974]  {:unknown, 'h'}
[15975]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[15979]  {:unknown, 't'}
[15980]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[15984]  {:unknown, 't'}
[15985]  {:unknown, 'h'}
[15986]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[15990]  {:unknown, 't'}
[15991]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[15995]  {:unknown, 'l'}
[15996]  {:unknown, 't'}
[15997]  {:unknown, ' '}
[15998]  {:unknown, 'L'}
[15999]  {:unknown, 'o'}
[16000]  {:unknown, 'c'}
[16001]  {:unknown, 'k'}
[16002]  {:unknown, [128]}
[16003]  {:unknown, 'Y'}
[16004]  {:unknown, 'o'}
[16005]  {:unknown, 'u'}
[16006]  {:unknown, ' '}
[16007]  {:unknown, 'a'}
[16008]  {:unknown, 'r'}
[16009]  {:unknown, 'e'}
[16010]  {:unknown, ' '}
[16011]  {:unknown, 'i'}
[16012]  {:unknown, 'n'}
[16013]  {:unknown, ' '}
[16014]  {:unknown, 'a'}
[16015]  {:unknown, ' '}
[16016]  {:unknown, 'g'}
[16017]  {:unknown, 'r'}
[16018]  {:unknown, 'i'}
[16019]  {:unknown, 'd'}
[16020]  {:unknown, ' '}
[16021]  {:unknown, 'o'}
[16022]  {:unknown, 'f'}
[16023]  {:unknown, ' '}
[16024]  {:unknown, 'r'}
[16025]  {:unknown, 'o'}
[16026]  {:unknown, 'o'}
[16027]  {:unknown, 'm'}
[16028]  {:unknown, 's'}
[16029]  {:unknown, ' '}
[16030]  {:unknown, 't'}
[16031]  {:unknown, 'h'}
[16032]  {:unknown, 'a'}
[16033]  {:unknown, 't'}
[16034]  {:unknown, ' '}
[16035]  {:unknown, 'c'}
[16036]  {:unknown, 'o'}
[16037]  {:unknown, 'n'}
[16038]  {:unknown, 't'}
[16039]  {:unknown, 'r'}
[16040]  {:unknown, 'o'}
[16041]  {:unknown, 'l'}
[16042]  {:unknown, ' '}
[16043]  {:unknown, 't'}
[16044]  {:unknown, 'h'}
[16045]  {:unknown, 'e'}
[16046]  {:unknown, ' '}
[16047]  {:unknown, 'd'}
[16048]  {:unknown, 'o'}
[16049]  {:unknown, 'o'}
[16050]  {:unknown, 'r'}
[16051]  {:unknown, ' '}
[16052]  {:unknown, 't'}
[16053]  {:unknown, 'o'}
[16054]  {:unknown, ' '}
[16055]  {:unknown, 't'}
[16056]  {:unknown, 'h'}
[16057]  {:unknown, 'e'}
[16058]  {:unknown, ' '}
[16059]  {:unknown, 'v'}
[16060]  {:unknown, 'a'}
[16061]  {:unknown, 'u'}
[16062]  {:unknown, 'l'}
[16063]  {:unknown, 't'}
[16064]  {:unknown, '.'}
[16065]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[16069]  {:unknown, 'e'}
[16070]  {:unknown, ' '}
[16071]  {:unknown, 'f'}
[16072]  {:unknown, 'l'}
[16073]  {:unknown, 'o'}
[16074]  {:unknown, 'o'}
[16075]  {:unknown, 'r'}
[16076]  {:unknown, ' '}
[16077]  {:unknown, 'o'}
[16078]  {:unknown, 'f'}
[16079]  {:unknown, ' '}
[16080]  {:unknown, 't'}
[16081]  {:unknown, 'h'}
[16082]  {:unknown, 'i'}
[16083]  {:unknown, 's'}
[16084]  {:unknown, ' '}
[16085]  {:unknown, 'r'}
[16086]  {:unknown, 'o'}
[16087]  {:unknown, 'o'}
[16088]  {:unknown, 'm'}
[16089]  {:unknown, ' '}
[16090]  {:unknown, 'i'}
[16091]  {:unknown, 's'}
[16092]  {:unknown, ' '}
[16093]  {:unknown, 'a'}
[16094]  {:unknown, ' '}
[16095]  {:unknown, 'l'}
[16096]  {:unknown, 'a'}
[16097]  {:unknown, 'r'}
[16098]  {:unknown, 'g'}
[16099]  {:unknown, 'e'}
[16100]  {:unknown, ' '}
[16101]  {:unknown, 'm'}
[16102]  {:unknown, 'o'}
[16103]  {:unknown, 's'}
[16104]  {:unknown, 'a'}
[16105]  {:unknown, 'i'}
[16106]  {:unknown, 'c'}
[16107]  {:unknown, ' '}
[16108]  {:unknown, 'd'}
[16109]  {:unknown, 'e'}
[16110]  {:unknown, 'p'}
[16111]  {:unknown, 'i'}
[16112]  {:unknown, 'c'}
[16113]  {:unknown, 't'}
[16114]  {:unknown, 'i'}
[16115]  {:unknown, 'n'}
[16116]  {:unknown, 'g'}
[16117]  {:unknown, ' '}
[16118]  {:unknown, 'a'}
[16119]  {:unknown, ' '}
[16120]  {:unknown, '\''}
[16121]  {:unknown, '*'}
[16122]  {:unknown, '\''}
[16123]  {:unknown, ' '}
[16124]  {:unknown, 's'}
[16125]  {:unknown, 'y'}
[16126]  {:unknown, 'm'}
[16127]  {:unknown, 'b'}
[16128]  {:unknown, 'o'}
[16129]  {:unknown, 'l'}
[16130]  {:unknown, '.'}
[16131]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16135]  {:unknown, 't'}
[16136]  {:unknown, 'h'}
[16137]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16141]  {:unknown, 't'}
[16142]  {:unknown, 'h'}
[16143]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[16147]  {:unknown, 't'}
[16148]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[16152]  {:unknown, 'l'}
[16153]  {:unknown, 't'}
[16154]  {:unknown, ' '}
[16155]  {:unknown, 'L'}
[16156]  {:unknown, 'o'}
[16157]  {:unknown, 'c'}
[16158]  {:unknown, 'k'}
[16159]  {:unknown, [128]}
[16160]  {:unknown, 'Y'}
[16161]  {:unknown, 'o'}
[16162]  {:unknown, 'u'}
[16163]  {:unknown, ' '}
[16164]  {:unknown, 'a'}
[16165]  {:unknown, 'r'}
[16166]  {:unknown, 'e'}
[16167]  {:unknown, ' '}
[16168]  {:unknown, 'i'}
[16169]  {:unknown, 'n'}
[16170]  {:unknown, ' '}
[16171]  {:unknown, 'a'}
[16172]  {:unknown, ' '}
[16173]  {:unknown, 'g'}
[16174]  {:unknown, 'r'}
[16175]  {:unknown, 'i'}
[16176]  {:unknown, 'd'}
[16177]  {:unknown, ' '}
[16178]  {:unknown, 'o'}
[16179]  {:unknown, 'f'}
[16180]  {:unknown, ' '}
[16181]  {:unknown, 'r'}
[16182]  {:unknown, 'o'}
[16183]  {:unknown, 'o'}
[16184]  {:unknown, 'm'}
[16185]  {:unknown, 's'}
[16186]  {:unknown, ' '}
[16187]  {:unknown, 't'}
[16188]  {:unknown, 'h'}
[16189]  {:unknown, 'a'}
[16190]  {:unknown, 't'}
[16191]  {:unknown, ' '}
[16192]  {:unknown, 'c'}
[16193]  {:unknown, 'o'}
[16194]  {:unknown, 'n'}
[16195]  {:unknown, 't'}
[16196]  {:unknown, 'r'}
[16197]  {:unknown, 'o'}
[16198]  {:unknown, 'l'}
[16199]  {:unknown, ' '}
[16200]  {:unknown, 't'}
[16201]  {:unknown, 'h'}
[16202]  {:unknown, 'e'}
[16203]  {:unknown, ' '}
[16204]  {:unknown, 'd'}
[16205]  {:unknown, 'o'}
[16206]  {:unknown, 'o'}
[16207]  {:unknown, 'r'}
[16208]  {:unknown, ' '}
[16209]  {:unknown, 't'}
[16210]  {:unknown, 'o'}
[16211]  {:unknown, ' '}
[16212]  {:unknown, 't'}
[16213]  {:unknown, 'h'}
[16214]  {:unknown, 'e'}
[16215]  {:unknown, ' '}
[16216]  {:unknown, 'v'}
[16217]  {:unknown, 'a'}
[16218]  {:unknown, 'u'}
[16219]  {:unknown, 'l'}
[16220]  {:unknown, 't'}
[16221]  {:unknown, '.'}
[16222]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[16226]  {:unknown, 'e'}
[16227]  {:unknown, ' '}
[16228]  {:unknown, 'f'}
[16229]  {:unknown, 'l'}
[16230]  {:unknown, 'o'}
[16231]  {:unknown, 'o'}
[16232]  {:unknown, 'r'}
[16233]  {:unknown, ' '}
[16234]  {:unknown, 'o'}
[16235]  {:unknown, 'f'}
[16236]  {:unknown, ' '}
[16237]  {:unknown, 't'}
[16238]  {:unknown, 'h'}
[16239]  {:unknown, 'i'}
[16240]  {:unknown, 's'}
[16241]  {:unknown, ' '}
[16242]  {:unknown, 'r'}
[16243]  {:unknown, 'o'}
[16244]  {:unknown, 'o'}
[16245]  {:unknown, 'm'}
[16246]  {:unknown, ' '}
[16247]  {:unknown, 'i'}
[16248]  {:unknown, 's'}
[16249]  {:unknown, ' '}
[16250]  {:unknown, 'a'}
[16251]  {:unknown, ' '}
[16252]  {:unknown, 'l'}
[16253]  {:unknown, 'a'}
[16254]  {:unknown, 'r'}
[16255]  {:unknown, 'g'}
[16256]  {:unknown, 'e'}
[16257]  {:unknown, ' '}
[16258]  {:unknown, 'm'}
[16259]  {:unknown, 'o'}
[16260]  {:unknown, 's'}
[16261]  {:unknown, 'a'}
[16262]  {:unknown, 'i'}
[16263]  {:unknown, 'c'}
[16264]  {:unknown, ' '}
[16265]  {:unknown, 'd'}
[16266]  {:unknown, 'e'}
[16267]  {:unknown, 'p'}
[16268]  {:unknown, 'i'}
[16269]  {:unknown, 'c'}
[16270]  {:unknown, 't'}
[16271]  {:unknown, 'i'}
[16272]  {:unknown, 'n'}
[16273]  {:unknown, 'g'}
[16274]  {:unknown, ' '}
[16275]  {:unknown, 'a'}
[16276]  {:unknown, ' '}
[16277]  {:unknown, '\''}
[16278]  {:unknown, '+'}
[16279]  {:unknown, '\''}
[16280]  {:unknown, ' '}
[16281]  {:unknown, 's'}
[16282]  {:unknown, 'y'}
[16283]  {:unknown, 'm'}
[16284]  {:unknown, 'b'}
[16285]  {:unknown, 'o'}
[16286]  {:unknown, 'l'}
[16287]  {:unknown, '.'}
[16288]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16292]  {:unknown, 't'}
[16293]  {:unknown, 'h'}
[16294]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[16298]  {:unknown, 't'}
[16299]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16303]  {:unknown, 't'}
[16304]  {:unknown, 'h'}
[16305]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[16309]  {:unknown, 'l'}
[16310]  {:unknown, 't'}
[16311]  {:unknown, ' '}
[16312]  {:unknown, 'L'}
[16313]  {:unknown, 'o'}
[16314]  {:unknown, 'c'}
[16315]  {:unknown, 'k'}
[16316]  {:unknown, [130]}
[16317]  {:unknown, 'Y'}
[16318]  {:unknown, 'o'}
[16319]  {:unknown, 'u'}
[16320]  {:unknown, ' '}
[16321]  {:unknown, 'a'}
[16322]  {:unknown, 'r'}
[16323]  {:unknown, 'e'}
[16324]  {:unknown, ' '}
[16325]  {:unknown, 'i'}
[16326]  {:unknown, 'n'}
[16327]  {:unknown, ' '}
[16328]  {:unknown, 'a'}
[16329]  {:unknown, ' '}
[16330]  {:unknown, 'g'}
[16331]  {:unknown, 'r'}
[16332]  {:unknown, 'i'}
[16333]  {:unknown, 'd'}
[16334]  {:unknown, ' '}
[16335]  {:unknown, 'o'}
[16336]  {:unknown, 'f'}
[16337]  {:unknown, ' '}
[16338]  {:unknown, 'r'}
[16339]  {:unknown, 'o'}
[16340]  {:unknown, 'o'}
[16341]  {:unknown, 'm'}
[16342]  {:unknown, 's'}
[16343]  {:unknown, ' '}
[16344]  {:unknown, 't'}
[16345]  {:unknown, 'h'}
[16346]  {:unknown, 'a'}
[16347]  {:unknown, 't'}
[16348]  {:unknown, ' '}
[16349]  {:unknown, 'c'}
[16350]  {:unknown, 'o'}
[16351]  {:unknown, 'n'}
[16352]  {:unknown, 't'}
[16353]  {:unknown, 'r'}
[16354]  {:unknown, 'o'}
[16355]  {:unknown, 'l'}
[16356]  {:unknown, ' '}
[16357]  {:unknown, 't'}
[16358]  {:unknown, 'h'}
[16359]  {:unknown, 'e'}
[16360]  {:unknown, ' '}
[16361]  {:unknown, 'd'}
[16362]  {:unknown, 'o'}
[16363]  {:unknown, 'o'}
[16364]  {:unknown, 'r'}
[16365]  {:unknown, ' '}
[16366]  {:unknown, 't'}
[16367]  {:unknown, 'o'}
[16368]  {:unknown, ' '}
[16369]  {:unknown, 't'}
[16370]  {:unknown, 'h'}
[16371]  {:unknown, 'e'}
[16372]  {:unknown, ' '}
[16373]  {:unknown, 'v'}
[16374]  {:unknown, 'a'}
[16375]  {:unknown, 'u'}
[16376]  {:unknown, 'l'}
[16377]  {:unknown, 't'}
[16378]  {:unknown, '.'}
[16379]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[16383]  {:unknown, 'e'}
[16384]  {:unknown, ' '}
[16385]  {:unknown, 'f'}
[16386]  {:unknown, 'l'}
[16387]  {:unknown, 'o'}
[16388]  {:unknown, 'o'}
[16389]  {:unknown, 'r'}
[16390]  {:unknown, ' '}
[16391]  {:unknown, 'o'}
[16392]  {:unknown, 'f'}
[16393]  {:unknown, ' '}
[16394]  {:unknown, 't'}
[16395]  {:unknown, 'h'}
[16396]  {:unknown, 'i'}
[16397]  {:unknown, 's'}
[16398]  {:unknown, ' '}
[16399]  {:unknown, 'r'}
[16400]  {:unknown, 'o'}
[16401]  {:unknown, 'o'}
[16402]  {:unknown, 'm'}
[16403]  {:unknown, ' '}
[16404]  {:unknown, 'i'}
[16405]  {:unknown, 's'}
[16406]  {:unknown, ' '}
[16407]  {:unknown, 'a'}
[16408]  {:unknown, ' '}
[16409]  {:unknown, 'l'}
[16410]  {:unknown, 'a'}
[16411]  {:unknown, 'r'}
[16412]  {:unknown, 'g'}
[16413]  {:unknown, 'e'}
[16414]  {:unknown, ' '}
[16415]  {:unknown, 'm'}
[16416]  {:unknown, 'o'}
[16417]  {:unknown, 's'}
[16418]  {:unknown, 'a'}
[16419]  {:unknown, 'i'}
[16420]  {:unknown, 'c'}
[16421]  {:unknown, ' '}
[16422]  {:unknown, 'd'}
[16423]  {:unknown, 'e'}
[16424]  {:unknown, 'p'}
[16425]  {:unknown, 'i'}
[16426]  {:unknown, 'c'}
[16427]  {:unknown, 't'}
[16428]  {:unknown, 'i'}
[16429]  {:unknown, 'n'}
[16430]  {:unknown, 'g'}
[16431]  {:unknown, ' '}
[16432]  {:unknown, 't'}
[16433]  {:unknown, 'h'}
[16434]  {:unknown, 'e'}
[16435]  {:unknown, ' '}
[16436]  {:unknown, 'n'}
[16437]  {:unknown, 'u'}
[16438]  {:unknown, 'm'}
[16439]  {:unknown, 'b'}
[16440]  {:unknown, 'e'}
[16441]  {:unknown, 'r'}
[16442]  {:unknown, ' '}
[16443]  {:unknown, '\''}
[16444]  {:unknown, '4'}
[16445]  {:unknown, '\''}
[16446]  {:unknown, '.'}
[16447]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16451]  {:unknown, 't'}
[16452]  {:unknown, 'h'}
[16453]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[16457]  {:unknown, 't'}
[16458]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16462]  {:unknown, 't'}
[16463]  {:unknown, 'h'}
[16464]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[16468]  {:unknown, 't'}
[16469]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[16473]  {:unknown, 'l'}
[16474]  {:unknown, 't'}
[16475]  {:unknown, ' '}
[16476]  {:unknown, 'L'}
[16477]  {:unknown, 'o'}
[16478]  {:unknown, 'c'}
[16479]  {:unknown, 'k'}
[16480]  {:unknown, [128]}
[16481]  {:unknown, 'Y'}
[16482]  {:unknown, 'o'}
[16483]  {:unknown, 'u'}
[16484]  {:unknown, ' '}
[16485]  {:unknown, 'a'}
[16486]  {:unknown, 'r'}
[16487]  {:unknown, 'e'}
[16488]  {:unknown, ' '}
[16489]  {:unknown, 'i'}
[16490]  {:unknown, 'n'}
[16491]  {:unknown, ' '}
[16492]  {:unknown, 'a'}
[16493]  {:unknown, ' '}
[16494]  {:unknown, 'g'}
[16495]  {:unknown, 'r'}
[16496]  {:unknown, 'i'}
[16497]  {:unknown, 'd'}
[16498]  {:unknown, ' '}
[16499]  {:unknown, 'o'}
[16500]  {:unknown, 'f'}
[16501]  {:unknown, ' '}
[16502]  {:unknown, 'r'}
[16503]  {:unknown, 'o'}
[16504]  {:unknown, 'o'}
[16505]  {:unknown, 'm'}
[16506]  {:unknown, 's'}
[16507]  {:unknown, ' '}
[16508]  {:unknown, 't'}
[16509]  {:unknown, 'h'}
[16510]  {:unknown, 'a'}
[16511]  {:unknown, 't'}
[16512]  {:unknown, ' '}
[16513]  {:unknown, 'c'}
[16514]  {:unknown, 'o'}
[16515]  {:unknown, 'n'}
[16516]  {:unknown, 't'}
[16517]  {:unknown, 'r'}
[16518]  {:unknown, 'o'}
[16519]  {:unknown, 'l'}
[16520]  {:unknown, ' '}
[16521]  {:unknown, 't'}
[16522]  {:unknown, 'h'}
[16523]  {:unknown, 'e'}
[16524]  {:unknown, ' '}
[16525]  {:unknown, 'd'}
[16526]  {:unknown, 'o'}
[16527]  {:unknown, 'o'}
[16528]  {:unknown, 'r'}
[16529]  {:unknown, ' '}
[16530]  {:unknown, 't'}
[16531]  {:unknown, 'o'}
[16532]  {:unknown, ' '}
[16533]  {:unknown, 't'}
[16534]  {:unknown, 'h'}
[16535]  {:unknown, 'e'}
[16536]  {:unknown, ' '}
[16537]  {:unknown, 'v'}
[16538]  {:unknown, 'a'}
[16539]  {:unknown, 'u'}
[16540]  {:unknown, 'l'}
[16541]  {:unknown, 't'}
[16542]  {:unknown, '.'}
[16543]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[16547]  {:unknown, 'e'}
[16548]  {:unknown, ' '}
[16549]  {:unknown, 'f'}
[16550]  {:unknown, 'l'}
[16551]  {:unknown, 'o'}
[16552]  {:unknown, 'o'}
[16553]  {:unknown, 'r'}
[16554]  {:unknown, ' '}
[16555]  {:unknown, 'o'}
[16556]  {:unknown, 'f'}
[16557]  {:unknown, ' '}
[16558]  {:unknown, 't'}
[16559]  {:unknown, 'h'}
[16560]  {:unknown, 'i'}
[16561]  {:unknown, 's'}
[16562]  {:unknown, ' '}
[16563]  {:unknown, 'r'}
[16564]  {:unknown, 'o'}
[16565]  {:unknown, 'o'}
[16566]  {:unknown, 'm'}
[16567]  {:unknown, ' '}
[16568]  {:unknown, 'i'}
[16569]  {:unknown, 's'}
[16570]  {:unknown, ' '}
[16571]  {:unknown, 'a'}
[16572]  {:unknown, ' '}
[16573]  {:unknown, 'l'}
[16574]  {:unknown, 'a'}
[16575]  {:unknown, 'r'}
[16576]  {:unknown, 'g'}
[16577]  {:unknown, 'e'}
[16578]  {:unknown, ' '}
[16579]  {:unknown, 'm'}
[16580]  {:unknown, 'o'}
[16581]  {:unknown, 's'}
[16582]  {:unknown, 'a'}
[16583]  {:unknown, 'i'}
[16584]  {:unknown, 'c'}
[16585]  {:unknown, ' '}
[16586]  {:unknown, 'd'}
[16587]  {:unknown, 'e'}
[16588]  {:unknown, 'p'}
[16589]  {:unknown, 'i'}
[16590]  {:unknown, 'c'}
[16591]  {:unknown, 't'}
[16592]  {:unknown, 'i'}
[16593]  {:unknown, 'n'}
[16594]  {:unknown, 'g'}
[16595]  {:unknown, ' '}
[16596]  {:unknown, 'a'}
[16597]  {:unknown, ' '}
[16598]  {:unknown, '\''}
[16599]  {:unknown, '-'}
[16600]  {:unknown, '\''}
[16601]  {:unknown, ' '}
[16602]  {:unknown, 's'}
[16603]  {:unknown, 'y'}
[16604]  {:unknown, 'm'}
[16605]  {:unknown, 'b'}
[16606]  {:unknown, 'o'}
[16607]  {:unknown, 'l'}
[16608]  {:unknown, '.'}
[16609]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16613]  {:unknown, 't'}
[16614]  {:unknown, 'h'}
[16615]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[16619]  {:unknown, 't'}
[16620]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16624]  {:unknown, 't'}
[16625]  {:unknown, 'h'}
[16626]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[16630]  {:unknown, 't'}
[16631]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[16635]  {:unknown, 'l'}
[16636]  {:unknown, 't'}
[16637]  {:unknown, ' '}
[16638]  {:unknown, 'L'}
[16639]  {:unknown, 'o'}
[16640]  {:unknown, 'c'}
[16641]  {:unknown, 'k'}
[16642]  {:unknown, [131]}
[16643]  {:unknown, 'Y'}
[16644]  {:unknown, 'o'}
[16645]  {:unknown, 'u'}
[16646]  {:unknown, ' '}
[16647]  {:unknown, 'a'}
[16648]  {:unknown, 'r'}
[16649]  {:unknown, 'e'}
[16650]  {:unknown, ' '}
[16651]  {:unknown, 'i'}
[16652]  {:unknown, 'n'}
[16653]  {:unknown, ' '}
[16654]  {:unknown, 'a'}
[16655]  {:unknown, ' '}
[16656]  {:unknown, 'g'}
[16657]  {:unknown, 'r'}
[16658]  {:unknown, 'i'}
[16659]  {:unknown, 'd'}
[16660]  {:unknown, ' '}
[16661]  {:unknown, 'o'}
[16662]  {:unknown, 'f'}
[16663]  {:unknown, ' '}
[16664]  {:unknown, 'r'}
[16665]  {:unknown, 'o'}
[16666]  {:unknown, 'o'}
[16667]  {:unknown, 'm'}
[16668]  {:unknown, 's'}
[16669]  {:unknown, ' '}
[16670]  {:unknown, 't'}
[16671]  {:unknown, 'h'}
[16672]  {:unknown, 'a'}
[16673]  {:unknown, 't'}
[16674]  {:unknown, ' '}
[16675]  {:unknown, 'c'}
[16676]  {:unknown, 'o'}
[16677]  {:unknown, 'n'}
[16678]  {:unknown, 't'}
[16679]  {:unknown, 'r'}
[16680]  {:unknown, 'o'}
[16681]  {:unknown, 'l'}
[16682]  {:unknown, ' '}
[16683]  {:unknown, 't'}
[16684]  {:unknown, 'h'}
[16685]  {:unknown, 'e'}
[16686]  {:unknown, ' '}
[16687]  {:unknown, 'd'}
[16688]  {:unknown, 'o'}
[16689]  {:unknown, 'o'}
[16690]  {:unknown, 'r'}
[16691]  {:unknown, ' '}
[16692]  {:unknown, 't'}
[16693]  {:unknown, 'o'}
[16694]  {:unknown, ' '}
[16695]  {:unknown, 't'}
[16696]  {:unknown, 'h'}
[16697]  {:unknown, 'e'}
[16698]  {:unknown, ' '}
[16699]  {:unknown, 'v'}
[16700]  {:unknown, 'a'}
[16701]  {:unknown, 'u'}
[16702]  {:unknown, 'l'}
[16703]  {:unknown, 't'}
[16704]  {:unknown, '.'}
[16705]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[16709]  {:unknown, 'e'}
[16710]  {:unknown, ' '}
[16711]  {:unknown, 'f'}
[16712]  {:unknown, 'l'}
[16713]  {:unknown, 'o'}
[16714]  {:unknown, 'o'}
[16715]  {:unknown, 'r'}
[16716]  {:unknown, ' '}
[16717]  {:unknown, 'o'}
[16718]  {:unknown, 'f'}
[16719]  {:unknown, ' '}
[16720]  {:unknown, 't'}
[16721]  {:unknown, 'h'}
[16722]  {:unknown, 'i'}
[16723]  {:unknown, 's'}
[16724]  {:unknown, ' '}
[16725]  {:unknown, 'r'}
[16726]  {:unknown, 'o'}
[16727]  {:unknown, 'o'}
[16728]  {:unknown, 'm'}
[16729]  {:unknown, ' '}
[16730]  {:unknown, 'i'}
[16731]  {:unknown, 's'}
[16732]  {:unknown, ' '}
[16733]  {:unknown, 'a'}
[16734]  {:unknown, ' '}
[16735]  {:unknown, 'l'}
[16736]  {:unknown, 'a'}
[16737]  {:unknown, 'r'}
[16738]  {:unknown, 'g'}
[16739]  {:unknown, 'e'}
[16740]  {:unknown, ' '}
[16741]  {:unknown, 'm'}
[16742]  {:unknown, 'o'}
[16743]  {:unknown, 's'}
[16744]  {:unknown, 'a'}
[16745]  {:unknown, 'i'}
[16746]  {:unknown, 'c'}
[16747]  {:unknown, ' '}
[16748]  {:unknown, 'd'}
[16749]  {:unknown, 'e'}
[16750]  {:unknown, 'p'}
[16751]  {:unknown, 'i'}
[16752]  {:unknown, 'c'}
[16753]  {:unknown, 't'}
[16754]  {:unknown, 'i'}
[16755]  {:unknown, 'n'}
[16756]  {:unknown, 'g'}
[16757]  {:unknown, ' '}
[16758]  {:unknown, 't'}
[16759]  {:unknown, 'h'}
[16760]  {:unknown, 'e'}
[16761]  {:unknown, ' '}
[16762]  {:unknown, 'n'}
[16763]  {:unknown, 'u'}
[16764]  {:unknown, 'm'}
[16765]  {:unknown, 'b'}
[16766]  {:unknown, 'e'}
[16767]  {:unknown, 'r'}
[16768]  {:unknown, ' '}
[16769]  {:unknown, '\''}
[16770]  {:unknown, '1'}
[16771]  {:unknown, '8'}
[16772]  {:unknown, '\''}
[16773]  {:unknown, '.'}
[16774]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16778]  {:unknown, 't'}
[16779]  {:unknown, 'h'}
[16780]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16784]  {:unknown, 't'}
[16785]  {:unknown, 'h'}
[16786]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[16790]  {:unknown, 't'}
[16791]  {:call, {:value, 86}}
[16793]  {:unknown, 'a'}
[16794]  {:unknown, 'u'}
[16795]  {:unknown, 'l'}
[16796]  {:unknown, 't'}
[16797]  {:unknown, ' '}
[16798]  {:unknown, 'A'}
[16799]  {:unknown, 'n'}
[16800]  {:unknown, 't'}
[16801]  {:unknown, 'e'}
[16802]  {:unknown, 'c'}
[16803]  {:unknown, 'h'}
[16804]  {:unknown, 'a'}
[16805]  {:unknown, 'm'}
[16806]  {:unknown, 'b'}
[16807]  {:unknown, 'e'}
[16808]  {:unknown, 'r'}
[16809]  {:unknown, [144]}
[16810]  {:unknown, 'Y'}
[16811]  {:unknown, 'o'}
[16812]  {:unknown, 'u'}
[16813]  {:unknown, ' '}
[16814]  {:unknown, 'a'}
[16815]  {:unknown, 'r'}
[16816]  {:unknown, 'e'}
[16817]  {:unknown, ' '}
[16818]  {:unknown, 'i'}
[16819]  {:unknown, 'n'}
[16820]  {:unknown, ' '}
[16821]  {:unknown, 't'}
[16822]  {:unknown, 'h'}
[16823]  {:unknown, 'e'}
[16824]  {:unknown, ' '}
[16825]  {:unknown, 'a'}
[16826]  {:unknown, 'n'}
[16827]  {:unknown, 't'}
[16828]  {:unknown, 'e'}
[16829]  {:unknown, 'c'}
[16830]  {:unknown, 'h'}
[16831]  {:unknown, 'a'}
[16832]  {:unknown, 'm'}
[16833]  {:unknown, 'b'}
[16834]  {:unknown, 'e'}
[16835]  {:unknown, 'r'}
[16836]  {:unknown, ' '}
[16837]  {:unknown, 't'}
[16838]  {:unknown, 'o'}
[16839]  {:unknown, ' '}
[16840]  {:unknown, 'a'}
[16841]  {:unknown, ' '}
[16842]  {:unknown, 'g'}
[16843]  {:unknown, 'r'}
[16844]  {:unknown, 'i'}
[16845]  {:unknown, 'd'}
[16846]  {:unknown, ' '}
[16847]  {:unknown, 'o'}
[16848]  {:unknown, 'f'}
[16849]  {:unknown, ' '}
[16850]  {:unknown, 'r'}
[16851]  {:unknown, 'o'}
[16852]  {:unknown, 'o'}
[16853]  {:unknown, 'm'}
[16854]  {:unknown, 's'}
[16855]  {:unknown, ' '}
[16856]  {:unknown, 't'}
[16857]  {:unknown, 'h'}
[16858]  {:unknown, 'a'}
[16859]  {:unknown, 't'}
[16860]  {:unknown, ' '}
[16861]  {:unknown, 'c'}
[16862]  {:unknown, 'o'}
[16863]  {:unknown, 'n'}
[16864]  {:unknown, 't'}
[16865]  {:unknown, 'r'}
[16866]  {:unknown, 'o'}
[16867]  {:unknown, 'l'}
[16868]  {:unknown, ' '}
[16869]  {:unknown, 't'}
[16870]  {:unknown, 'h'}
[16871]  {:unknown, 'e'}
[16872]  {:unknown, ' '}
[16873]  {:unknown, 'd'}
[16874]  {:unknown, 'o'}
[16875]  {:unknown, 'o'}
[16876]  {:unknown, 'r'}
[16877]  {:unknown, ' '}
[16878]  {:unknown, 't'}
[16879]  {:unknown, 'o'}
[16880]  {:unknown, ' '}
[16881]  {:unknown, 't'}
[16882]  {:unknown, 'h'}
[16883]  {:unknown, 'e'}
[16884]  {:unknown, ' '}
[16885]  {:unknown, 'v'}
[16886]  {:unknown, 'a'}
[16887]  {:unknown, 'u'}
[16888]  {:unknown, 'l'}
[16889]  {:unknown, 't'}
[16890]  {:unknown, '.'}
[16891]  {:unknown, ' '}
[16892]  {:unknown, ' '}
[16893]  {:unknown, 'Y'}
[16894]  {:unknown, 'o'}
[16895]  {:unknown, 'u'}
[16896]  {:unknown, ' '}
[16897]  {:unknown, 'n'}
[16898]  {:unknown, 'o'}
[16899]  {:unknown, 't'}
[16900]  {:unknown, 'i'}
[16901]  {:unknown, 'c'}
[16902]  {:unknown, 'e'}
[16903]  {:unknown, ' '}
[16904]  {:unknown, 't'}
[16905]  {:unknown, 'h'}
[16906]  {:unknown, 'e'}
[16907]  {:unknown, ' '}
[16908]  {:unknown, 'n'}
[16909]  {:unknown, 'u'}
[16910]  {:unknown, 'm'}
[16911]  {:unknown, 'b'}
[16912]  {:unknown, 'e'}
[16913]  {:unknown, 'r'}
[16914]  {:unknown, ' '}
[16915]  {:unknown, '\''}
[16916]  {:unknown, '2'}
[16917]  {:unknown, '2'}
[16918]  {:unknown, '\''}
[16919]  {:unknown, ' '}
[16920]  {:unknown, 'i'}
[16921]  {:unknown, 's'}
[16922]  {:unknown, ' '}
[16923]  {:unknown, 'c'}
[16924]  {:unknown, 'a'}
[16925]  {:unknown, 'r'}
[16926]  {:unknown, 'v'}
[16927]  {:unknown, 'e'}
[16928]  {:unknown, 'd'}
[16929]  {:unknown, ' '}
[16930]  {:unknown, 'i'}
[16931]  {:unknown, 'n'}
[16932]  {:unknown, 't'}
[16933]  {:unknown, 'o'}
[16934]  {:unknown, ' '}
[16935]  {:unknown, 't'}
[16936]  {:unknown, 'h'}
[16937]  {:unknown, 'e'}
[16938]  {:unknown, ' '}
[16939]  {:unknown, 'o'}
[16940]  {:unknown, 'r'}
[16941]  {:unknown, 'b'}
[16942]  {:unknown, '\''}
[16943]  {:unknown, 's'}
[16944]  {:unknown, ' '}
[16945]  {:unknown, 'p'}
[16946]  {:unknown, 'e'}
[16947]  {:unknown, 'd'}
[16948]  {:unknown, 'e'}
[16949]  {:unknown, 's'}
[16950]  {:unknown, 't'}
[16951]  {:unknown, 'a'}
[16952]  {:unknown, 'l'}
[16953]  {:unknown, '.'}
[16954]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[16958]  {:unknown, 't'}
[16959]  {:unknown, 'h'}
[16960]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[16964]  {:unknown, 't'}
[16965]  {:gt, {:value, 115}, {:value, 111}, {:value, 117}}
[16969]  {:unknown, 't'}
[16970]  {:unknown, 'h'}
[16971]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[16975]  {:unknown, 'l'}
[16976]  {:unknown, 't'}
[16977]  {:unknown, ' '}
[16978]  {:unknown, 'L'}
[16979]  {:unknown, 'o'}
[16980]  {:unknown, 'c'}
[16981]  {:unknown, 'k'}
[16982]  {:unknown, [128]}
[16983]  {:unknown, 'Y'}
[16984]  {:unknown, 'o'}
[16985]  {:unknown, 'u'}
[16986]  {:unknown, ' '}
[16987]  {:unknown, 'a'}
[16988]  {:unknown, 'r'}
[16989]  {:unknown, 'e'}
[16990]  {:unknown, ' '}
[16991]  {:unknown, 'i'}
[16992]  {:unknown, 'n'}
[16993]  {:unknown, ' '}
[16994]  {:unknown, 'a'}
[16995]  {:unknown, ' '}
[16996]  {:unknown, 'g'}
[16997]  {:unknown, 'r'}
[16998]  {:unknown, 'i'}
[16999]  {:unknown, 'd'}
[17000]  {:unknown, ' '}
[17001]  {:unknown, 'o'}
[17002]  {:unknown, 'f'}
[17003]  {:unknown, ' '}
[17004]  {:unknown, 'r'}
[17005]  {:unknown, 'o'}
[17006]  {:unknown, 'o'}
[17007]  {:unknown, 'm'}
[17008]  {:unknown, 's'}
[17009]  {:unknown, ' '}
[17010]  {:unknown, 't'}
[17011]  {:unknown, 'h'}
[17012]  {:unknown, 'a'}
[17013]  {:unknown, 't'}
[17014]  {:unknown, ' '}
[17015]  {:unknown, 'c'}
[17016]  {:unknown, 'o'}
[17017]  {:unknown, 'n'}
[17018]  {:unknown, 't'}
[17019]  {:unknown, 'r'}
[17020]  {:unknown, 'o'}
[17021]  {:unknown, 'l'}
[17022]  {:unknown, ' '}
[17023]  {:unknown, 't'}
[17024]  {:unknown, 'h'}
[17025]  {:unknown, 'e'}
[17026]  {:unknown, ' '}
[17027]  {:unknown, 'd'}
[17028]  {:unknown, 'o'}
[17029]  {:unknown, 'o'}
[17030]  {:unknown, 'r'}
[17031]  {:unknown, ' '}
[17032]  {:unknown, 't'}
[17033]  {:unknown, 'o'}
[17034]  {:unknown, ' '}
[17035]  {:unknown, 't'}
[17036]  {:unknown, 'h'}
[17037]  {:unknown, 'e'}
[17038]  {:unknown, ' '}
[17039]  {:unknown, 'v'}
[17040]  {:unknown, 'a'}
[17041]  {:unknown, 'u'}
[17042]  {:unknown, 'l'}
[17043]  {:unknown, 't'}
[17044]  {:unknown, '.'}
[17045]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[17049]  {:unknown, 'e'}
[17050]  {:unknown, ' '}
[17051]  {:unknown, 'f'}
[17052]  {:unknown, 'l'}
[17053]  {:unknown, 'o'}
[17054]  {:unknown, 'o'}
[17055]  {:unknown, 'r'}
[17056]  {:unknown, ' '}
[17057]  {:unknown, 'o'}
[17058]  {:unknown, 'f'}
[17059]  {:unknown, ' '}
[17060]  {:unknown, 't'}
[17061]  {:unknown, 'h'}
[17062]  {:unknown, 'i'}
[17063]  {:unknown, 's'}
[17064]  {:unknown, ' '}
[17065]  {:unknown, 'r'}
[17066]  {:unknown, 'o'}
[17067]  {:unknown, 'o'}
[17068]  {:unknown, 'm'}
[17069]  {:unknown, ' '}
[17070]  {:unknown, 'i'}
[17071]  {:unknown, 's'}
[17072]  {:unknown, ' '}
[17073]  {:unknown, 'a'}
[17074]  {:unknown, ' '}
[17075]  {:unknown, 'l'}
[17076]  {:unknown, 'a'}
[17077]  {:unknown, 'r'}
[17078]  {:unknown, 'g'}
[17079]  {:unknown, 'e'}
[17080]  {:unknown, ' '}
[17081]  {:unknown, 'm'}
[17082]  {:unknown, 'o'}
[17083]  {:unknown, 's'}
[17084]  {:unknown, 'a'}
[17085]  {:unknown, 'i'}
[17086]  {:unknown, 'c'}
[17087]  {:unknown, ' '}
[17088]  {:unknown, 'd'}
[17089]  {:unknown, 'e'}
[17090]  {:unknown, 'p'}
[17091]  {:unknown, 'i'}
[17092]  {:unknown, 'c'}
[17093]  {:unknown, 't'}
[17094]  {:unknown, 'i'}
[17095]  {:unknown, 'n'}
[17096]  {:unknown, 'g'}
[17097]  {:unknown, ' '}
[17098]  {:unknown, 'a'}
[17099]  {:unknown, ' '}
[17100]  {:unknown, '\''}
[17101]  {:unknown, '-'}
[17102]  {:unknown, '\''}
[17103]  {:unknown, ' '}
[17104]  {:unknown, 's'}
[17105]  {:unknown, 'y'}
[17106]  {:unknown, 'm'}
[17107]  {:unknown, 'b'}
[17108]  {:unknown, 'o'}
[17109]  {:unknown, 'l'}
[17110]  {:unknown, '.'}
[17111]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[17115]  {:unknown, 't'}
[17116]  {:unknown, 'h'}
[17117]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[17121]  {:unknown, 't'}
[17122]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[17126]  {:unknown, 't'}
[17127]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[17131]  {:unknown, 'l'}
[17132]  {:unknown, 't'}
[17133]  {:unknown, ' '}
[17134]  {:unknown, 'L'}
[17135]  {:unknown, 'o'}
[17136]  {:unknown, 'c'}
[17137]  {:unknown, 'k'}
[17138]  {:unknown, [130]}
[17139]  {:unknown, 'Y'}
[17140]  {:unknown, 'o'}
[17141]  {:unknown, 'u'}
[17142]  {:unknown, ' '}
[17143]  {:unknown, 'a'}
[17144]  {:unknown, 'r'}
[17145]  {:unknown, 'e'}
[17146]  {:unknown, ' '}
[17147]  {:unknown, 'i'}
[17148]  {:unknown, 'n'}
[17149]  {:unknown, ' '}
[17150]  {:unknown, 'a'}
[17151]  {:unknown, ' '}
[17152]  {:unknown, 'g'}
[17153]  {:unknown, 'r'}
[17154]  {:unknown, 'i'}
[17155]  {:unknown, 'd'}
[17156]  {:unknown, ' '}
[17157]  {:unknown, 'o'}
[17158]  {:unknown, 'f'}
[17159]  {:unknown, ' '}
[17160]  {:unknown, 'r'}
[17161]  {:unknown, 'o'}
[17162]  {:unknown, 'o'}
[17163]  {:unknown, 'm'}
[17164]  {:unknown, 's'}
[17165]  {:unknown, ' '}
[17166]  {:unknown, 't'}
[17167]  {:unknown, 'h'}
[17168]  {:unknown, 'a'}
[17169]  {:unknown, 't'}
[17170]  {:unknown, ' '}
[17171]  {:unknown, 'c'}
[17172]  {:unknown, 'o'}
[17173]  {:unknown, 'n'}
[17174]  {:unknown, 't'}
[17175]  {:unknown, 'r'}
[17176]  {:unknown, 'o'}
[17177]  {:unknown, 'l'}
[17178]  {:unknown, ' '}
[17179]  {:unknown, 't'}
[17180]  {:unknown, 'h'}
[17181]  {:unknown, 'e'}
[17182]  {:unknown, ' '}
[17183]  {:unknown, 'd'}
[17184]  {:unknown, 'o'}
[17185]  {:unknown, 'o'}
[17186]  {:unknown, 'r'}
[17187]  {:unknown, ' '}
[17188]  {:unknown, 't'}
[17189]  {:unknown, 'o'}
[17190]  {:unknown, ' '}
[17191]  {:unknown, 't'}
[17192]  {:unknown, 'h'}
[17193]  {:unknown, 'e'}
[17194]  {:unknown, ' '}
[17195]  {:unknown, 'v'}
[17196]  {:unknown, 'a'}
[17197]  {:unknown, 'u'}
[17198]  {:unknown, 'l'}
[17199]  {:unknown, 't'}
[17200]  {:unknown, '.'}
[17201]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[17205]  {:unknown, 'e'}
[17206]  {:unknown, ' '}
[17207]  {:unknown, 'f'}
[17208]  {:unknown, 'l'}
[17209]  {:unknown, 'o'}
[17210]  {:unknown, 'o'}
[17211]  {:unknown, 'r'}
[17212]  {:unknown, ' '}
[17213]  {:unknown, 'o'}
[17214]  {:unknown, 'f'}
[17215]  {:unknown, ' '}
[17216]  {:unknown, 't'}
[17217]  {:unknown, 'h'}
[17218]  {:unknown, 'i'}
[17219]  {:unknown, 's'}
[17220]  {:unknown, ' '}
[17221]  {:unknown, 'r'}
[17222]  {:unknown, 'o'}
[17223]  {:unknown, 'o'}
[17224]  {:unknown, 'm'}
[17225]  {:unknown, ' '}
[17226]  {:unknown, 'i'}
[17227]  {:unknown, 's'}
[17228]  {:unknown, ' '}
[17229]  {:unknown, 'a'}
[17230]  {:unknown, ' '}
[17231]  {:unknown, 'l'}
[17232]  {:unknown, 'a'}
[17233]  {:unknown, 'r'}
[17234]  {:unknown, 'g'}
[17235]  {:unknown, 'e'}
[17236]  {:unknown, ' '}
[17237]  {:unknown, 'm'}
[17238]  {:unknown, 'o'}
[17239]  {:unknown, 's'}
[17240]  {:unknown, 'a'}
[17241]  {:unknown, 'i'}
[17242]  {:unknown, 'c'}
[17243]  {:unknown, ' '}
[17244]  {:unknown, 'd'}
[17245]  {:unknown, 'e'}
[17246]  {:unknown, 'p'}
[17247]  {:unknown, 'i'}
[17248]  {:unknown, 'c'}
[17249]  {:unknown, 't'}
[17250]  {:unknown, 'i'}
[17251]  {:unknown, 'n'}
[17252]  {:unknown, 'g'}
[17253]  {:unknown, ' '}
[17254]  {:unknown, 't'}
[17255]  {:unknown, 'h'}
[17256]  {:unknown, 'e'}
[17257]  {:unknown, ' '}
[17258]  {:unknown, 'n'}
[17259]  {:unknown, 'u'}
[17260]  {:unknown, 'm'}
[17261]  {:unknown, 'b'}
[17262]  {:unknown, 'e'}
[17263]  {:unknown, 'r'}
[17264]  {:unknown, ' '}
[17265]  {:unknown, '\''}
[17266]  {:unknown, '9'}
[17267]  {:unknown, '\''}
[17268]  {:unknown, '.'}
[17269]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[17273]  {:unknown, 't'}
[17274]  {:unknown, 'h'}
[17275]  {:eq, {:value, 101}, {:value, 97}, {:value, 115}}
[17279]  {:unknown, 't'}
[17280]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[17284]  {:unknown, 't'}
[17285]  {:mult, {:value, 86}, {:value, 97}, {:value, 117}}
[17289]  {:unknown, 'l'}
[17290]  {:unknown, 't'}
[17291]  {:unknown, ' '}
[17292]  {:unknown, 'L'}
[17293]  {:unknown, 'o'}
[17294]  {:unknown, 'c'}
[17295]  {:unknown, 'k'}
[17296]  {:unknown, [128]}
[17297]  {:unknown, 'Y'}
[17298]  {:unknown, 'o'}
[17299]  {:unknown, 'u'}
[17300]  {:unknown, ' '}
[17301]  {:unknown, 'a'}
[17302]  {:unknown, 'r'}
[17303]  {:unknown, 'e'}
[17304]  {:unknown, ' '}
[17305]  {:unknown, 'i'}
[17306]  {:unknown, 'n'}
[17307]  {:unknown, ' '}
[17308]  {:unknown, 'a'}
[17309]  {:unknown, ' '}
[17310]  {:unknown, 'g'}
[17311]  {:unknown, 'r'}
[17312]  {:unknown, 'i'}
[17313]  {:unknown, 'd'}
[17314]  {:unknown, ' '}
[17315]  {:unknown, 'o'}
[17316]  {:unknown, 'f'}
[17317]  {:unknown, ' '}
[17318]  {:unknown, 'r'}
[17319]  {:unknown, 'o'}
[17320]  {:unknown, 'o'}
[17321]  {:unknown, 'm'}
[17322]  {:unknown, 's'}
[17323]  {:unknown, ' '}
[17324]  {:unknown, 't'}
[17325]  {:unknown, 'h'}
[17326]  {:unknown, 'a'}
[17327]  {:unknown, 't'}
[17328]  {:unknown, ' '}
[17329]  {:unknown, 'c'}
[17330]  {:unknown, 'o'}
[17331]  {:unknown, 'n'}
[17332]  {:unknown, 't'}
[17333]  {:unknown, 'r'}
[17334]  {:unknown, 'o'}
[17335]  {:unknown, 'l'}
[17336]  {:unknown, ' '}
[17337]  {:unknown, 't'}
[17338]  {:unknown, 'h'}
[17339]  {:unknown, 'e'}
[17340]  {:unknown, ' '}
[17341]  {:unknown, 'd'}
[17342]  {:unknown, 'o'}
[17343]  {:unknown, 'o'}
[17344]  {:unknown, 'r'}
[17345]  {:unknown, ' '}
[17346]  {:unknown, 't'}
[17347]  {:unknown, 'o'}
[17348]  {:unknown, ' '}
[17349]  {:unknown, 't'}
[17350]  {:unknown, 'h'}
[17351]  {:unknown, 'e'}
[17352]  {:unknown, ' '}
[17353]  {:unknown, 'v'}
[17354]  {:unknown, 'a'}
[17355]  {:unknown, 'u'}
[17356]  {:unknown, 'l'}
[17357]  {:unknown, 't'}
[17358]  {:unknown, '.'}
[17359]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[17363]  {:unknown, 'e'}
[17364]  {:unknown, ' '}
[17365]  {:unknown, 'f'}
[17366]  {:unknown, 'l'}
[17367]  {:unknown, 'o'}
[17368]  {:unknown, 'o'}
[17369]  {:unknown, 'r'}
[17370]  {:unknown, ' '}
[17371]  {:unknown, 'o'}
[17372]  {:unknown, 'f'}
[17373]  {:unknown, ' '}
[17374]  {:unknown, 't'}
[17375]  {:unknown, 'h'}
[17376]  {:unknown, 'i'}
[17377]  {:unknown, 's'}
[17378]  {:unknown, ' '}
[17379]  {:unknown, 'r'}
[17380]  {:unknown, 'o'}
[17381]  {:unknown, 'o'}
[17382]  {:unknown, 'm'}
[17383]  {:unknown, ' '}
[17384]  {:unknown, 'i'}
[17385]  {:unknown, 's'}
[17386]  {:unknown, ' '}
[17387]  {:unknown, 'a'}
[17388]  {:unknown, ' '}
[17389]  {:unknown, 'l'}
[17390]  {:unknown, 'a'}
[17391]  {:unknown, 'r'}
[17392]  {:unknown, 'g'}
[17393]  {:unknown, 'e'}
[17394]  {:unknown, ' '}
[17395]  {:unknown, 'm'}
[17396]  {:unknown, 'o'}
[17397]  {:unknown, 's'}
[17398]  {:unknown, 'a'}
[17399]  {:unknown, 'i'}
[17400]  {:unknown, 'c'}
[17401]  {:unknown, ' '}
[17402]  {:unknown, 'd'}
[17403]  {:unknown, 'e'}
[17404]  {:unknown, 'p'}
[17405]  {:unknown, 'i'}
[17406]  {:unknown, 'c'}
[17407]  {:unknown, 't'}
[17408]  {:unknown, 'i'}
[17409]  {:unknown, 'n'}
[17410]  {:unknown, 'g'}
[17411]  {:unknown, ' '}
[17412]  {:unknown, 'a'}
[17413]  {:unknown, ' '}
[17414]  {:unknown, '\''}
[17415]  {:unknown, '*'}
[17416]  {:unknown, '\''}
[17417]  {:unknown, ' '}
[17418]  {:unknown, 's'}
[17419]  {:unknown, 'y'}
[17420]  {:unknown, 'm'}
[17421]  {:unknown, 'b'}
[17422]  {:unknown, 'o'}
[17423]  {:unknown, 'l'}
[17424]  {:unknown, '.'}
[17425]  {:gt, {:value, 110}, {:value, 111}, {:value, 114}}
[17429]  {:unknown, 't'}
[17430]  {:unknown, 'h'}
[17431]  {:eq, {:value, 119}, {:value, 101}, {:value, 115}}
[17435]  {:unknown, 't'}
[17436]  {:gt, {:value, 86}, {:value, 97}, {:value, 117}}
[17440]  {:unknown, 'l'}
[17441]  {:unknown, 't'}
[17442]  {:unknown, [212]}
[17443]  {:unknown, 'T'}
[17444]  {:unknown, 'h'}
[17445]  {:unknown, 'i'}
[17446]  {:unknown, 's'}
[17447]  {:unknown, ' '}
[17448]  {:unknown, 'v'}
[17449]  {:unknown, 'a'}
[17450]  {:unknown, 'u'}
[17451]  {:unknown, 'l'}
[17452]  {:unknown, 't'}
[17453]  {:unknown, ' '}
[17454]  {:unknown, 'c'}
[17455]  {:unknown, 'o'}
[17456]  {:unknown, 'n'}
[17457]  {:unknown, 't'}
[17458]  {:unknown, 'a'}
[17459]  {:unknown, 'i'}
[17460]  {:unknown, 'n'}
[17461]  {:unknown, 's'}
[17462]  {:unknown, ' '}
[17463]  {:unknown, 'i'}
[17464]  {:unknown, 'n'}
[17465]  {:unknown, 'c'}
[17466]  {:unknown, 'r'}
[17467]  {:unknown, 'e'}
[17468]  {:unknown, 'd'}
[17469]  {:unknown, 'i'}
[17470]  {:unknown, 'b'}
[17471]  {:unknown, 'l'}
[17472]  {:unknown, 'e'}
[17473]  {:unknown, ' '}
[17474]  {:unknown, 'r'}
[17475]  {:unknown, 'i'}
[17476]  {:unknown, 'c'}
[17477]  {:unknown, 'h'}
[17478]  {:unknown, 'e'}
[17479]  {:unknown, 's'}
[17480]  {:unknown, '!'}
[17481]  {:unknown, ' '}
[17482]  {:unknown, ' '}
[17483]  {:unknown, 'P'}
[17484]  {:unknown, 'i'}
[17485]  {:unknown, 'l'}
[17486]  {:unknown, 'e'}
[17487]  {:unknown, 's'}
[17488]  {:unknown, ' '}
[17489]  {:unknown, 'o'}
[17490]  {:unknown, 'f'}
[17491]  {:unknown, ' '}
[17492]  {:unknown, 'g'}
[17493]  {:unknown, 'o'}
[17494]  {:unknown, 'l'}
[17495]  {:unknown, 'd'}
[17496]  {:unknown, ' '}
[17497]  {:unknown, 'a'}
[17498]  {:unknown, 'n'}
[17499]  {:unknown, 'd'}
[17500]  {:unknown, ' '}
[17501]  {:unknown, 'p'}
[17502]  {:unknown, 'l'}
[17503]  {:unknown, 'a'}
[17504]  {:unknown, 't'}
[17505]  {:unknown, 'i'}
[17506]  {:unknown, 'n'}
[17507]  {:unknown, 'u'}
[17508]  {:unknown, 'm'}
[17509]  {:unknown, ' '}
[17510]  {:unknown, 'c'}
[17511]  {:unknown, 'o'}
[17512]  {:unknown, 'i'}
[17513]  {:unknown, 'n'}
[17514]  {:unknown, 's'}
[17515]  {:unknown, ' '}
[17516]  {:unknown, 's'}
[17517]  {:unknown, 'u'}
[17518]  {:unknown, 'r'}
[17519]  {:unknown, 'r'}
[17520]  {:unknown, 'o'}
[17521]  {:unknown, 'u'}
[17522]  {:unknown, 'n'}
[17523]  {:unknown, 'd'}
[17524]  {:unknown, ' '}
[17525]  {:unknown, 'y'}
[17526]  {:unknown, 'o'}
[17527]  {:unknown, 'u'}
[17528]  {:unknown, ','}
[17529]  {:unknown, ' '}
[17530]  {:unknown, 'a'}
[17531]  {:unknown, 'n'}
[17532]  {:unknown, 'd'}
[17533]  {:unknown, ' '}
[17534]  {:unknown, 't'}
[17535]  {:unknown, 'h'}
[17536]  {:unknown, 'e'}
[17537]  {:unknown, ' '}
[17538]  {:unknown, 'w'}
[17539]  {:unknown, 'a'}
[17540]  {:unknown, 'l'}
[17541]  {:unknown, 'l'}
[17542]  {:unknown, 's'}
[17543]  {:unknown, ' '}
[17544]  {:unknown, 'a'}
[17545]  {:unknown, 'r'}
[17546]  {:unknown, 'e'}
[17547]  {:unknown, ' '}
[17548]  {:unknown, 'a'}
[17549]  {:unknown, 'd'}
[17550]  {:unknown, 'o'}
[17551]  {:unknown, 'r'}
[17552]  {:unknown, 'n'}
[17553]  {:unknown, 'e'}
[17554]  {:unknown, 'd'}
[17555]  {:unknown, ' '}
[17556]  {:unknown, 'w'}
[17557]  {:unknown, 'i'}
[17558]  {:unknown, 't'}
[17559]  {:unknown, 'h'}
[17560]  {:unknown, ' '}
[17561]  {:unknown, 't'}
[17562]  {:unknown, 'o'}
[17563]  {:unknown, 'p'}
[17564]  {:unknown, 'a'}
[17565]  {:unknown, 'z'}
[17566]  {:unknown, 'e'}
[17567]  {:unknown, 's'}
[17568]  {:unknown, ','}
[17569]  {:unknown, ' '}
[17570]  {:unknown, 'r'}
[17571]  {:unknown, 'u'}
[17572]  {:unknown, 'b'}
[17573]  {:unknown, 'i'}
[17574]  {:unknown, 'e'}
[17575]  {:unknown, 's'}
[17576]  {:unknown, ','}
[17577]  {:unknown, ' '}
[17578]  {:unknown, 's'}
[17579]  {:unknown, 'a'}
[17580]  {:unknown, 'p'}
[17581]  {:unknown, 'p'}
[17582]  {:unknown, 'h'}
[17583]  {:unknown, 'i'}
[17584]  {:unknown, 'r'}
[17585]  {:unknown, 'e'}
[17586]  {:unknown, 's'}
[17587]  {:unknown, ','}
[17588]  {:unknown, ' '}
[17589]  {:unknown, 'e'}
[17590]  {:unknown, 'm'}
[17591]  {:unknown, 'e'}
[17592]  {:unknown, 'r'}
[17593]  {:unknown, 'a'}
[17594]  {:unknown, 'l'}
[17595]  {:unknown, 'd'}
[17596]  {:unknown, 's'}
[17597]  {:unknown, ','}
[17598]  {:unknown, ' '}
[17599]  {:unknown, 'o'}
[17600]  {:unknown, 'p'}
[17601]  {:unknown, 'a'}
[17602]  {:unknown, 'l'}
[17603]  {:unknown, 's'}
[17604]  {:unknown, ','}
[17605]  {:unknown, ' '}
[17606]  {:unknown, 'd'}
[17607]  {:unknown, 'i'}
[17608]  {:unknown, 'l'}
[17609]  {:unknown, 'i'}
[17610]  {:unknown, 't'}
[17611]  {:unknown, 'h'}
[17612]  {:unknown, 'i'}
[17613]  {:unknown, 'u'}
[17614]  {:unknown, 'm'}
[17615]  {:unknown, ' '}
[17616]  {:unknown, 'c'}
[17617]  {:unknown, 'r'}
[17618]  {:unknown, 'y'}
[17619]  {:unknown, 's'}
[17620]  {:unknown, 't'}
[17621]  {:unknown, 'a'}
[17622]  {:unknown, 'l'}
[17623]  {:unknown, 's'}
[17624]  {:unknown, ','}
[17625]  {:unknown, ' '}
[17626]  {:unknown, 'e'}
[17627]  {:unknown, 'l'}
[17628]  {:unknown, 'e'}
[17629]  {:unknown, 'r'}
[17630]  {:unknown, 'i'}
[17631]  {:unknown, 'u'}
[17632]  {:unknown, 'm'}
[17633]  {:unknown, '-'}
[17634]  {:unknown, '1'}
[17635]  {:unknown, '1'}
[17636]  {:unknown, '5'}
[17637]  {:unknown, ','}
[17638]  {:unknown, ' '}
[17639]  {:unknown, 'a'}
[17640]  {:unknown, 'n'}
[17641]  {:unknown, 'd'}
[17642]  {:unknown, ' '}
[17643]  {:unknown, 'u'}
[17644]  {:unknown, 'n'}
[17645]  {:unknown, 'o'}
[17646]  {:unknown, 'b'}
[17647]  {:unknown, 't'}
[17648]  {:unknown, 'a'}
[17649]  {:unknown, 'i'}
[17650]  {:unknown, 'n'}
[17651]  {:unknown, 'i'}
[17652]  {:unknown, 'u'}
[17653]  {:unknown, 'm'}
[17654]  {:unknown, '.'}
[17655]  {:gt, {:value, 108}, {:value, 101}, {:value, 97}}
[17659]  {:unknown, 'v'}
[17660]  {:unknown, 'e'}
[17661]  {:unknown, [31]}
[17662]  {:unknown, 'F'}
[17663]  {:unknown, 'u'}
[17664]  {:unknown, 'm'}
[17665]  {:unknown, 'b'}
[17666]  {:unknown, 'l'}
[17667]  {:unknown, 'i'}
[17668]  {:unknown, 'n'}
[17669]  {:unknown, 'g'}
[17670]  {:unknown, ' '}
[17671]  {:unknown, 'a'}
[17672]  {:unknown, 'r'}
[17673]  {:unknown, 'o'}
[17674]  {:unknown, 'u'}
[17675]  {:unknown, 'n'}
[17676]  {:unknown, 'd'}
[17677]  {:unknown, ' '}
[17678]  {:unknown, 'i'}
[17679]  {:unknown, 'n'}
[17680]  {:unknown, ' '}
[17681]  {:unknown, 't'}
[17682]  {:unknown, 'h'}
[17683]  {:unknown, 'e'}
[17684]  {:unknown, ' '}
[17685]  {:unknown, 'd'}
[17686]  {:unknown, 'a'}
[17687]  {:unknown, 'r'}
[17688]  {:unknown, 'k'}
[17689]  {:unknown, 'n'}
[17690]  {:unknown, 'e'}
[17691]  {:unknown, 's'}
[17692]  {:unknown, 's'}
[17693]  {:unknown, 'c'}
[17694]  {:unknown, 'W'}
[17695]  {:unknown, 'i'}
[17696]  {:unknown, 't'}
[17697]  {:unknown, 'h'}
[17698]  {:unknown, 'o'}
[17699]  {:unknown, 'u'}
[17700]  {:unknown, 't'}
[17701]  {:unknown, ' '}
[17702]  {:unknown, 'a'}
[17703]  {:unknown, ' '}
[17704]  {:unknown, 's'}
[17705]  {:unknown, 'o'}
[17706]  {:unknown, 'u'}
[17707]  {:unknown, 'r'}
[17708]  {:unknown, 'c'}
[17709]  {:unknown, 'e'}
[17710]  {:unknown, ' '}
[17711]  {:unknown, 'o'}
[17712]  {:unknown, 'f'}
[17713]  {:unknown, ' '}
[17714]  {:unknown, 'l'}
[17715]  {:unknown, 'i'}
[17716]  {:unknown, 'g'}
[17717]  {:unknown, 'h'}
[17718]  {:unknown, 't'}
[17719]  {:unknown, ','}
[17720]  {:unknown, ' '}
[17721]  {:unknown, 'y'}
[17722]  {:unknown, 'o'}
[17723]  {:unknown, 'u'}
[17724]  {:unknown, ' '}
[17725]  {:unknown, 'h'}
[17726]  {:unknown, 'a'}
[17727]  {:unknown, 'v'}
[17728]  {:unknown, 'e'}
[17729]  {:unknown, ' '}
[17730]  {:unknown, 'b'}
[17731]  {:unknown, 'e'}
[17732]  {:unknown, 'c'}
[17733]  {:unknown, 'o'}
[17734]  {:unknown, 'm'}
[17735]  {:unknown, 'e'}
[17736]  {:unknown, ' '}
[17737]  {:unknown, 'h'}
[17738]  {:unknown, 'o'}
[17739]  {:unknown, 'p'}
[17740]  {:unknown, 'e'}
[17741]  {:unknown, 'l'}
[17742]  {:unknown, 'e'}
[17743]  {:unknown, 's'}
[17744]  {:unknown, 's'}
[17745]  {:unknown, 'l'}
[17746]  {:unknown, 'y'}
[17747]  {:unknown, ' '}
[17748]  {:unknown, 'l'}
[17749]  {:unknown, 'o'}
[17750]  {:unknown, 's'}
[17751]  {:unknown, 't'}
[17752]  {:unknown, ' '}
[17753]  {:unknown, 'a'}
[17754]  {:unknown, 'n'}
[17755]  {:unknown, 'd'}
[17756]  {:unknown, ' '}
[17757]  {:unknown, 'a'}
[17758]  {:unknown, 'r'}
[17759]  {:unknown, 'e'}
[17760]  {:unknown, ' '}
[17761]  {:unknown, 'f'}
[17762]  {:unknown, 'u'}
[17763]  {:unknown, 'm'}
[17764]  {:unknown, 'b'}
[17765]  {:unknown, 'l'}
[17766]  {:unknown, 'i'}
[17767]  {:unknown, 'n'}
[17768]  {:unknown, 'g'}
[17769]  {:unknown, ' '}
[17770]  {:unknown, 'a'}
[17771]  {:unknown, 'r'}
[17772]  {:unknown, 'o'}
[17773]  {:unknown, 'u'}
[17774]  {:unknown, 'n'}
[17775]  {:unknown, 'd'}
[17776]  {:unknown, ' '}
[17777]  {:unknown, 'i'}
[17778]  {:unknown, 'n'}
[17779]  {:unknown, ' '}
[17780]  {:unknown, 't'}
[17781]  {:unknown, 'h'}
[17782]  {:unknown, 'e'}
[17783]  {:unknown, ' '}
[17784]  {:unknown, 'd'}
[17785]  {:unknown, 'a'}
[17786]  {:unknown, 'r'}
[17787]  {:unknown, 'k'}
[17788]  {:unknown, 'n'}
[17789]  {:unknown, 'e'}
[17790]  {:unknown, 's'}
[17791]  {:unknown, 's'}
[17792]  {:unknown, '.'}
[17793]  {:jt, {:value, 102}, {:value, 111}}
[17796]  {:unknown, 'r'}
[17797]  {:unknown, 'w'}
[17798]  {:unknown, 'a'}
[17799]  {:unknown, 'r'}
[17800]  {:unknown, 'd'}
[17801]  {:eq, {:value, 98}, {:value, 97}, {:value, 99}}
[17805]  {:unknown, 'k'}
[17806]  {:unknown, [31]}
[17807]  {:unknown, 'F'}
[17808]  {:unknown, 'u'}
[17809]  {:unknown, 'm'}
[17810]  {:unknown, 'b'}
[17811]  {:unknown, 'l'}
[17812]  {:unknown, 'i'}
[17813]  {:unknown, 'n'}
[17814]  {:unknown, 'g'}
[17815]  {:unknown, ' '}
[17816]  {:unknown, 'a'}
[17817]  {:unknown, 'r'}
[17818]  {:unknown, 'o'}
[17819]  {:unknown, 'u'}
[17820]  {:unknown, 'n'}
[17821]  {:unknown, 'd'}
[17822]  {:unknown, ' '}
[17823]  {:unknown, 'i'}
[17824]  {:unknown, 'n'}
[17825]  {:unknown, ' '}
[17826]  {:unknown, 't'}
[17827]  {:unknown, 'h'}
[17828]  {:unknown, 'e'}
[17829]  {:unknown, ' '}
[17830]  {:unknown, 'd'}
[17831]  {:unknown, 'a'}
[17832]  {:unknown, 'r'}
[17833]  {:unknown, 'k'}
[17834]  {:unknown, 'n'}
[17835]  {:unknown, 'e'}
[17836]  {:unknown, 's'}
[17837]  {:unknown, 's'}
[17838]  {:unknown, '^'}
[17839]  {:unknown, 'Y'}
[17840]  {:unknown, 'o'}
[17841]  {:unknown, 'u'}
[17842]  {:unknown, ' '}
[17843]  {:unknown, 'c'}
[17844]  {:unknown, 'a'}
[17845]  {:unknown, 'n'}
[17846]  {:unknown, '\''}
[17847]  {:unknown, 't'}
[17848]  {:unknown, ' '}
[17849]  {:unknown, 's'}
[17850]  {:unknown, 'e'}
[17851]  {:unknown, 'e'}
[17852]  {:unknown, 'm'}
[17853]  {:unknown, ' '}
[17854]  {:unknown, 't'}
[17855]  {:unknown, 'o'}
[17856]  {:unknown, ' '}
[17857]  {:unknown, 'f'}
[17858]  {:unknown, 'i'}
[17859]  {:unknown, 'n'}
[17860]  {:unknown, 'd'}
[17861]  {:unknown, ' '}
[17862]  {:unknown, 'y'}
[17863]  {:unknown, 'o'}
[17864]  {:unknown, 'u'}
[17865]  {:unknown, 'r'}
[17866]  {:unknown, ' '}
[17867]  {:unknown, 'w'}
[17868]  {:unknown, 'a'}
[17869]  {:unknown, 'y'}
[17870]  {:unknown, '.'}
[17871]  {:unknown, ' '}
[17872]  {:unknown, ' '}
[17873]  {:unknown, 'H'}
[17874]  {:unknown, 'o'}
[17875]  {:unknown, 'w'}
[17876]  {:unknown, 'e'}
[17877]  {:unknown, 'v'}
[17878]  {:unknown, 'e'}
[17879]  {:unknown, 'r'}
[17880]  {:unknown, ','}
[17881]  {:unknown, ' '}
[17882]  {:unknown, 'y'}
[17883]  {:unknown, 'o'}
[17884]  {:unknown, 'u'}
[17885]  {:unknown, ' '}
[17886]  {:unknown, 'd'}
[17887]  {:unknown, 'o'}
[17888]  {:unknown, ' '}
[17889]  {:unknown, 't'}
[17890]  {:unknown, 'h'}
[17891]  {:unknown, 'i'}
[17892]  {:unknown, 'n'}
[17893]  {:unknown, 'k'}
[17894]  {:unknown, ' '}
[17895]  {:unknown, 'y'}
[17896]  {:unknown, 'o'}
[17897]  {:unknown, 'u'}
[17898]  {:unknown, ' '}
[17899]  {:unknown, 'h'}
[17900]  {:unknown, 'e'}
[17901]  {:unknown, 'a'}
[17902]  {:unknown, 'r'}
[17903]  {:unknown, ' '}
[17904]  {:unknown, 'l'}
[17905]  {:unknown, 'o'}
[17906]  {:unknown, 'w'}
[17907]  {:unknown, ' '}
[17908]  {:unknown, 'g'}
[17909]  {:unknown, 'r'}
[17910]  {:unknown, 'o'}
[17911]  {:unknown, 'w'}
[17912]  {:unknown, 'l'}
[17913]  {:unknown, 'i'}
[17914]  {:unknown, 'n'}
[17915]  {:unknown, 'g'}
[17916]  {:unknown, ' '}
[17917]  {:unknown, 'i'}
[17918]  {:unknown, 'n'}
[17919]  {:unknown, ' '}
[17920]  {:unknown, 't'}
[17921]  {:unknown, 'h'}
[17922]  {:unknown, 'e'}
[17923]  {:unknown, ' '}
[17924]  {:unknown, 'd'}
[17925]  {:unknown, 'i'}
[17926]  {:unknown, 's'}
[17927]  {:unknown, 't'}
[17928]  {:unknown, 'a'}
[17929]  {:unknown, 'n'}
[17930]  {:unknown, 'c'}
[17931]  {:unknown, 'e'}
[17932]  {:unknown, '.'}
[17933]  {:pop, {:value, 114}}
[17935]  {:unknown, 'u'}
[17936]  {:unknown, 'n'}
[17937]  {:mod, {:value, 105}, {:value, 110}, {:value, 118}}
[17941]  {:unknown, 'e'}
[17942]  {:unknown, 's'}
[17943]  {:unknown, 't'}
[17944]  {:unknown, 'i'}
[17945]  {:unknown, 'g'}
[17946]  {:unknown, 'a'}
[17947]  {:unknown, 't'}
[17948]  {:unknown, 'e'}
[17949]  {:call, {:value, 80}}
[17951]  {:unknown, 'a'}
[17952]  {:unknown, 'n'}
[17953]  {:unknown, 'i'}
[17954]  {:unknown, 'c'}
[17955]  {:unknown, 'k'}
[17956]  {:unknown, 'e'}
[17957]  {:unknown, 'd'}
[17958]  {:unknown, ' '}
[17959]  {:unknown, 'a'}
[17960]  {:unknown, 'n'}
[17961]  {:unknown, 'd'}
[17962]  {:unknown, ' '}
[17963]  {:unknown, 'l'}
[17964]  {:unknown, 'o'}
[17965]  {:unknown, 's'}
[17966]  {:unknown, 't'}
[17967]  {:unknown, 'I'}
[17968]  {:unknown, 'T'}
[17969]  {:unknown, 'h'}
[17970]  {:unknown, 'e'}
[17971]  {:unknown, ' '}
[17972]  {:unknown, 'g'}
[17973]  {:unknown, 'r'}
[17974]  {:unknown, 'o'}
[17975]  {:unknown, 'w'}
[17976]  {:unknown, 'l'}
[17977]  {:unknown, 'i'}
[17978]  {:unknown, 'n'}
[17979]  {:unknown, 'g'}
[17980]  {:unknown, ' '}
[17981]  {:unknown, 'i'}
[17982]  {:unknown, 's'}
[17983]  {:unknown, ' '}
[17984]  {:unknown, 'g'}
[17985]  {:unknown, 'e'}
[17986]  {:unknown, 't'}
[17987]  {:unknown, 't'}
[17988]  {:unknown, 'i'}
[17989]  {:unknown, 'n'}
[17990]  {:unknown, 'g'}
[17991]  {:unknown, ' '}
[17992]  {:unknown, 'l'}
[17993]  {:unknown, 'o'}
[17994]  {:unknown, 'u'}
[17995]  {:unknown, 'd'}
[17996]  {:unknown, 'e'}
[17997]  {:unknown, 'r'}
[17998]  {:unknown, '.'}
[17999]  {:unknown, ' '}
[18000]  {:unknown, ' '}
[18001]  {:unknown, 'D'}
[18002]  {:unknown, 'i'}
[18003]  {:unknown, 'd'}
[18004]  {:unknown, ' '}
[18005]  {:unknown, 'I'}
[18006]  {:unknown, ' '}
[18007]  {:unknown, 'm'}
[18008]  {:unknown, 'e'}
[18009]  {:unknown, 'n'}
[18010]  {:unknown, 't'}
[18011]  {:unknown, 'i'}
[18012]  {:unknown, 'o'}
[18013]  {:unknown, 'n'}
[18014]  {:unknown, ' '}
[18015]  {:unknown, 't'}
[18016]  {:unknown, 'h'}
[18017]  {:unknown, 'e'}
[18018]  {:unknown, 'r'}
[18019]  {:unknown, 'e'}
[18020]  {:unknown, ' '}
[18021]  {:unknown, 'a'}
[18022]  {:unknown, 'r'}
[18023]  {:unknown, 'e'}
[18024]  {:unknown, ' '}
[18025]  {:unknown, 'G'}
[18026]  {:unknown, 'r'}
[18027]  {:unknown, 'u'}
[18028]  {:unknown, 'e'}
[18029]  {:unknown, 's'}
[18030]  {:unknown, ' '}
[18031]  {:unknown, 'd'}
[18032]  {:unknown, 'o'}
[18033]  {:unknown, 'w'}
[18034]  {:unknown, 'n'}
[18035]  {:unknown, ' '}
[18036]  {:unknown, 'h'}
[18037]  {:unknown, 'e'}
[18038]  {:unknown, 'r'}
[18039]  {:unknown, 'e'}
[18040]  {:unknown, '?'}
[18041]  {:pop, {:value, 114}}
[18043]  {:unknown, 'u'}
[18044]  {:unknown, 'n'}
[18045]  {:eq, {:value, 119}, {:value, 97}, {:value, 105}}
[18049]  {:unknown, 't'}
[18050]  {:eq, {:value, 104}, {:value, 105}, {:value, 100}}
[18054]  {:unknown, 'e'}
[18055]  {:gt, {:value, 101}, {:value, 97}, {:value, 116}}
[18059]  {:unknown, 'e'}
[18060]  {:unknown, 'n'}
[18061]  {:halt}
[18062]  {:jmp, {:value, 116}}				# Tablet (start)
[18064]  {:unknown, 'a'}
[18065]  {:unknown, 'b'}
[18066]  {:unknown, 'l'}
[18067]  {:unknown, 'e'}
[18068]  {:unknown, 't'}
[18069]  {:unknown, [136]}				# Tablet description
[18070]  {:unknown, 'T'}
[18071]  {:unknown, 'h'}
[18072]  {:unknown, 'e'}
[18073]  {:unknown, ' '}
[18074]  {:unknown, 't'}
[18075]  {:unknown, 'a'}
[18076]  {:unknown, 'b'}
[18077]  {:unknown, 'l'}
[18078]  {:unknown, 'e'}
[18079]  {:unknown, 't'}
[18080]  {:unknown, ' '}
[18081]  {:unknown, 's'}
[18082]  {:unknown, 'e'}
[18083]  {:unknown, 'e'}
[18084]  {:unknown, 'm'}
[18085]  {:unknown, 's'}
[18086]  {:unknown, ' '}
[18087]  {:unknown, 'a'}
[18088]  {:unknown, 'p'}
[18089]  {:unknown, 'p'}
[18090]  {:unknown, 'r'}
[18091]  {:unknown, 'o'}
[18092]  {:unknown, 'p'}
[18093]  {:unknown, 'r'}
[18094]  {:unknown, 'i'}
[18095]  {:unknown, 'a'}
[18096]  {:unknown, 't'}
[18097]  {:unknown, 'e'}
[18098]  {:unknown, ' '}
[18099]  {:unknown, 'f'}
[18100]  {:unknown, 'o'}
[18101]  {:unknown, 'r'}
[18102]  {:unknown, ' '}
[18103]  {:unknown, 'u'}
[18104]  {:unknown, 's'}
[18105]  {:unknown, 'e'}
[18106]  {:unknown, ' '}
[18107]  {:unknown, 'a'}
[18108]  {:unknown, 's'}
[18109]  {:unknown, ' '}
[18110]  {:unknown, 'a'}
[18111]  {:unknown, ' '}
[18112]  {:unknown, 'w'}
[18113]  {:unknown, 'r'}
[18114]  {:unknown, 'i'}
[18115]  {:unknown, 't'}
[18116]  {:unknown, 'i'}
[18117]  {:unknown, 'n'}
[18118]  {:unknown, 'g'}
[18119]  {:unknown, ' '}
[18120]  {:unknown, 's'}
[18121]  {:unknown, 'u'}
[18122]  {:unknown, 'r'}
[18123]  {:unknown, 'f'}
[18124]  {:unknown, 'a'}
[18125]  {:unknown, 'c'}
[18126]  {:unknown, 'e'}
[18127]  {:unknown, ' '}
[18128]  {:unknown, 'b'}
[18129]  {:unknown, 'u'}
[18130]  {:unknown, 't'}
[18131]  {:unknown, ' '}
[18132]  {:unknown, 'i'}
[18133]  {:unknown, 's'}
[18134]  {:unknown, ' '}
[18135]  {:unknown, 'u'}
[18136]  {:unknown, 'n'}
[18137]  {:unknown, 'f'}
[18138]  {:unknown, 'o'}
[18139]  {:unknown, 'r'}
[18140]  {:unknown, 't'}
[18141]  {:unknown, 'u'}
[18142]  {:unknown, 'n'}
[18143]  {:unknown, 'a'}
[18144]  {:unknown, 't'}
[18145]  {:unknown, 'e'}
[18146]  {:unknown, 'l'}
[18147]  {:unknown, 'y'}
[18148]  {:unknown, ' '}
[18149]  {:unknown, 'b'}
[18150]  {:unknown, 'l'}
[18151]  {:unknown, 'a'}
[18152]  {:unknown, 'n'}
[18153]  {:unknown, 'k'}
[18154]  {:unknown, '.'}
[18155]  {:unknown, ' '}
[18156]  {:unknown, ' '}
[18157]  {:unknown, 'P'}
[18158]  {:unknown, 'e'}
[18159]  {:unknown, 'r'}
[18160]  {:unknown, 'h'}
[18161]  {:unknown, 'a'}
[18162]  {:unknown, 'p'}
[18163]  {:unknown, 's'}
[18164]  {:unknown, ' '}
[18165]  {:unknown, 'y'}
[18166]  {:unknown, 'o'}
[18167]  {:unknown, 'u'}
[18168]  {:unknown, ' '}
[18169]  {:unknown, 's'}
[18170]  {:unknown, 'h'}
[18171]  {:unknown, 'o'}
[18172]  {:unknown, 'u'}
[18173]  {:unknown, 'l'}
[18174]  {:unknown, 'd'}
[18175]  {:unknown, ' '}
[18176]  {:unknown, 'U'}
[18177]  {:unknown, 'S'}
[18178]  {:unknown, 'E'}
[18179]  {:unknown, ' '}
[18180]  {:unknown, 'i'}
[18181]  {:unknown, 't'}
[18182]  {:unknown, ' '}
[18183]  {:unknown, 'a'}
[18184]  {:unknown, 's'}
[18185]  {:unknown, ' '}
[18186]  {:unknown, 'a'}
[18187]  {:unknown, ' '}
[18188]  {:unknown, 'w'}
[18189]  {:unknown, 'r'}
[18190]  {:unknown, 'i'}
[18191]  {:unknown, 't'}
[18192]  {:unknown, 'i'}
[18193]  {:unknown, 'n'}
[18194]  {:unknown, 'g'}
[18195]  {:unknown, ' '}
[18196]  {:unknown, 's'}
[18197]  {:unknown, 'u'}
[18198]  {:unknown, 'r'}
[18199]  {:unknown, 'f'}
[18200]  {:unknown, 'a'}
[18201]  {:unknown, 'c'}
[18202]  {:unknown, 'e'}
[18203]  {:unknown, '.'}
[18204]  {:unknown, '.'}
[18205]  {:unknown, '.'}
[18206]  {:or, {:value, 101}, {:value, 109}, {:value, 112}}
[18210]  {:unknown, 't'}
[18211]  {:unknown, 'y'}
[18212]  {:unknown, ' '}
[18213]  {:unknown, 'l'}
[18214]  {:unknown, 'a'}
[18215]  {:unknown, 'n'}
[18216]  {:unknown, 't'}
[18217]  {:unknown, 'e'}
[18218]  {:unknown, 'r'}
[18219]  {:unknown, 'n'}
[18220]  {:unknown, '{'}
[18221]  {:unknown, 'T'}
[18222]  {:unknown, 'h'}
[18223]  {:unknown, 'e'}
[18224]  {:unknown, ' '}
[18225]  {:unknown, 'l'}
[18226]  {:unknown, 'a'}
[18227]  {:unknown, 'n'}
[18228]  {:unknown, 't'}
[18229]  {:unknown, 'e'}
[18230]  {:unknown, 'r'}
[18231]  {:unknown, 'n'}
[18232]  {:unknown, ' '}
[18233]  {:unknown, 's'}
[18234]  {:unknown, 'e'}
[18235]  {:unknown, 'e'}
[18236]  {:unknown, 'm'}
[18237]  {:unknown, 's'}
[18238]  {:unknown, ' '}
[18239]  {:unknown, 't'}
[18240]  {:unknown, 'o'}
[18241]  {:unknown, ' '}
[18242]  {:unknown, 'h'}
[18243]  {:unknown, 'a'}
[18244]  {:unknown, 'v'}
[18245]  {:unknown, 'e'}
[18246]  {:unknown, ' '}
[18247]  {:unknown, 'q'}
[18248]  {:unknown, 'u'}
[18249]  {:unknown, 'i'}
[18250]  {:unknown, 't'}
[18251]  {:unknown, 'e'}
[18252]  {:unknown, ' '}
[18253]  {:unknown, 'a'}
[18254]  {:unknown, ' '}
[18255]  {:unknown, 'b'}
[18256]  {:unknown, 'i'}
[18257]  {:unknown, 't'}
[18258]  {:unknown, ' '}
[18259]  {:unknown, 'o'}
[18260]  {:unknown, 'f'}
[18261]  {:unknown, ' '}
[18262]  {:unknown, 'w'}
[18263]  {:unknown, 'e'}
[18264]  {:unknown, 'a'}
[18265]  {:unknown, 'r'}
[18266]  {:unknown, ' '}
[18267]  {:unknown, 'b'}
[18268]  {:unknown, 'u'}
[18269]  {:unknown, 't'}
[18270]  {:unknown, ' '}
[18271]  {:unknown, 'a'}
[18272]  {:unknown, 'p'}
[18273]  {:unknown, 'p'}
[18274]  {:unknown, 'e'}
[18275]  {:unknown, 'a'}
[18276]  {:unknown, 'r'}
[18277]  {:unknown, 's'}
[18278]  {:unknown, ' '}
[18279]  {:unknown, 'o'}
[18280]  {:unknown, 't'}
[18281]  {:unknown, 'h'}
[18282]  {:unknown, 'e'}
[18283]  {:unknown, 'r'}
[18284]  {:unknown, 'w'}
[18285]  {:unknown, 'i'}
[18286]  {:unknown, 's'}
[18287]  {:unknown, 'e'}
[18288]  {:unknown, ' '}
[18289]  {:unknown, 'f'}
[18290]  {:unknown, 'u'}
[18291]  {:unknown, 'n'}
[18292]  {:unknown, 'c'}
[18293]  {:unknown, 't'}
[18294]  {:unknown, 'i'}
[18295]  {:unknown, 'o'}
[18296]  {:unknown, 'n'}
[18297]  {:unknown, 'a'}
[18298]  {:unknown, 'l'}
[18299]  {:unknown, '.'}
[18300]  {:unknown, ' '}
[18301]  {:unknown, ' '}
[18302]  {:unknown, 'I'}
[18303]  {:unknown, 't'}
[18304]  {:unknown, ' '}
[18305]  {:unknown, 'i'}
[18306]  {:unknown, 's'}
[18307]  {:unknown, ','}
[18308]  {:unknown, ' '}
[18309]  {:unknown, 'h'}
[18310]  {:unknown, 'o'}
[18311]  {:unknown, 'w'}
[18312]  {:unknown, 'e'}
[18313]  {:unknown, 'v'}
[18314]  {:unknown, 'e'}
[18315]  {:unknown, 'r'}
[18316]  {:unknown, ','}
[18317]  {:unknown, ' '}
[18318]  {:unknown, 's'}
[18319]  {:unknown, 'a'}
[18320]  {:unknown, 'd'}
[18321]  {:unknown, ' '}
[18322]  {:unknown, 't'}
[18323]  {:unknown, 'h'}
[18324]  {:unknown, 'a'}
[18325]  {:unknown, 't'}
[18326]  {:unknown, ' '}
[18327]  {:unknown, 'i'}
[18328]  {:unknown, 't'}
[18329]  {:unknown, ' '}
[18330]  {:unknown, 'i'}
[18331]  {:unknown, 's'}
[18332]  {:unknown, ' '}
[18333]  {:unknown, 'o'}
[18334]  {:unknown, 'u'}
[18335]  {:unknown, 't'}
[18336]  {:unknown, ' '}
[18337]  {:unknown, 'o'}
[18338]  {:unknown, 'f'}
[18339]  {:unknown, ' '}
[18340]  {:unknown, 'o'}
[18341]  {:unknown, 'i'}
[18342]  {:unknown, 'l'}
[18343]  {:unknown, '.'}
[18344]  {:jt, {:value, 108}, {:value, 97}}
[18347]  {:unknown, 'n'}
[18348]  {:unknown, 't'}
[18349]  {:unknown, 'e'}
[18350]  {:unknown, 'r'}
[18351]  {:unknown, 'n'}
[18352]  {:unknown, 's'}
[18353]  {:unknown, 'T'}
[18354]  {:unknown, 'h'}
[18355]  {:unknown, 'e'}
[18356]  {:unknown, ' '}
[18357]  {:unknown, 'l'}
[18358]  {:unknown, 'a'}
[18359]  {:unknown, 'n'}
[18360]  {:unknown, 't'}
[18361]  {:unknown, 'e'}
[18362]  {:unknown, 'r'}
[18363]  {:unknown, 'n'}
[18364]  {:unknown, ' '}
[18365]  {:unknown, 's'}
[18366]  {:unknown, 'e'}
[18367]  {:unknown, 'e'}
[18368]  {:unknown, 'm'}
[18369]  {:unknown, 's'}
[18370]  {:unknown, ' '}
[18371]  {:unknown, 't'}
[18372]  {:unknown, 'o'}
[18373]  {:unknown, ' '}
[18374]  {:unknown, 'h'}
[18375]  {:unknown, 'a'}
[18376]  {:unknown, 'v'}
[18377]  {:unknown, 'e'}
[18378]  {:unknown, ' '}
[18379]  {:unknown, 'q'}
[18380]  {:unknown, 'u'}
[18381]  {:unknown, 'i'}
[18382]  {:unknown, 't'}
[18383]  {:unknown, 'e'}
[18384]  {:unknown, ' '}
[18385]  {:unknown, 'a'}
[18386]  {:unknown, ' '}
[18387]  {:unknown, 'b'}
[18388]  {:unknown, 'i'}
[18389]  {:unknown, 't'}
[18390]  {:unknown, ' '}
[18391]  {:unknown, 'o'}
[18392]  {:unknown, 'f'}
[18393]  {:unknown, ' '}
[18394]  {:unknown, 'w'}
[18395]  {:unknown, 'e'}
[18396]  {:unknown, 'a'}
[18397]  {:unknown, 'r'}
[18398]  {:unknown, ' '}
[18399]  {:unknown, 'b'}
[18400]  {:unknown, 'u'}
[18401]  {:unknown, 't'}
[18402]  {:unknown, ' '}
[18403]  {:unknown, 'a'}
[18404]  {:unknown, 'p'}
[18405]  {:unknown, 'p'}
[18406]  {:unknown, 'e'}
[18407]  {:unknown, 'a'}
[18408]  {:unknown, 'r'}
[18409]  {:unknown, 's'}
[18410]  {:unknown, ' '}
[18411]  {:unknown, 'o'}
[18412]  {:unknown, 't'}
[18413]  {:unknown, 'h'}
[18414]  {:unknown, 'e'}
[18415]  {:unknown, 'r'}
[18416]  {:unknown, 'w'}
[18417]  {:unknown, 'i'}
[18418]  {:unknown, 's'}
[18419]  {:unknown, 'e'}
[18420]  {:unknown, ' '}
[18421]  {:unknown, 'f'}
[18422]  {:unknown, 'u'}
[18423]  {:unknown, 'n'}
[18424]  {:unknown, 'c'}
[18425]  {:unknown, 't'}
[18426]  {:unknown, 'i'}
[18427]  {:unknown, 'o'}
[18428]  {:unknown, 'n'}
[18429]  {:unknown, 'a'}
[18430]  {:unknown, 'l'}
[18431]  {:unknown, '.'}
[18432]  {:unknown, ' '}
[18433]  {:unknown, ' '}
[18434]  {:unknown, 'I'}
[18435]  {:unknown, 't'}
[18436]  {:unknown, ' '}
[18437]  {:unknown, 'i'}
[18438]  {:unknown, 's'}
[18439]  {:unknown, ' '}
[18440]  {:unknown, 'o'}
[18441]  {:unknown, 'f'}
[18442]  {:unknown, 'f'}
[18443]  {:unknown, ' '}
[18444]  {:unknown, 'b'}
[18445]  {:unknown, 'u'}
[18446]  {:unknown, 't'}
[18447]  {:unknown, ' '}
[18448]  {:unknown, 'h'}
[18449]  {:unknown, 'a'}
[18450]  {:unknown, 'p'}
[18451]  {:unknown, 'p'}
[18452]  {:unknown, 'i'}
[18453]  {:unknown, 'l'}
[18454]  {:unknown, 'y'}
[18455]  {:unknown, ' '}
[18456]  {:unknown, 'f'}
[18457]  {:unknown, 'u'}
[18458]  {:unknown, 'l'}
[18459]  {:unknown, 'l'}
[18460]  {:unknown, ' '}
[18461]  {:unknown, 'o'}
[18462]  {:unknown, 'f'}
[18463]  {:unknown, ' '}
[18464]  {:unknown, 'o'}
[18465]  {:unknown, 'i'}
[18466]  {:unknown, 'l'}
[18467]  {:unknown, '.'}
[18468]  {:mod, {:value, 108}, {:value, 105}, {:value, 116}}
[18472]  {:unknown, ' '}
[18473]  {:unknown, 'l'}
[18474]  {:unknown, 'a'}
[18475]  {:unknown, 'n'}
[18476]  {:unknown, 't'}
[18477]  {:unknown, 'e'}
[18478]  {:unknown, 'r'}
[18479]  {:unknown, 'n'}
[18480]  {:unknown, 'W'}
[18481]  {:unknown, 'T'}
[18482]  {:unknown, 'h'}
[18483]  {:unknown, 'e'}
[18484]  {:unknown, ' '}
[18485]  {:unknown, 'l'}
[18486]  {:unknown, 'a'}
[18487]  {:unknown, 'n'}
[18488]  {:unknown, 't'}
[18489]  {:unknown, 'e'}
[18490]  {:unknown, 'r'}
[18491]  {:unknown, 'n'}
[18492]  {:unknown, ' '}
[18493]  {:unknown, 's'}
[18494]  {:unknown, 'e'}
[18495]  {:unknown, 'e'}
[18496]  {:unknown, 'm'}
[18497]  {:unknown, 's'}
[18498]  {:unknown, ' '}
[18499]  {:unknown, 't'}
[18500]  {:unknown, 'o'}
[18501]  {:unknown, ' '}
[18502]  {:unknown, 'h'}
[18503]  {:unknown, 'a'}
[18504]  {:unknown, 'v'}
[18505]  {:unknown, 'e'}
[18506]  {:unknown, ' '}
[18507]  {:unknown, 'q'}
[18508]  {:unknown, 'u'}
[18509]  {:unknown, 'i'}
[18510]  {:unknown, 't'}
[18511]  {:unknown, 'e'}
[18512]  {:unknown, ' '}
[18513]  {:unknown, 'a'}
[18514]  {:unknown, ' '}
[18515]  {:unknown, 'b'}
[18516]  {:unknown, 'i'}
[18517]  {:unknown, 't'}
[18518]  {:unknown, ' '}
[18519]  {:unknown, 'o'}
[18520]  {:unknown, 'f'}
[18521]  {:unknown, ' '}
[18522]  {:unknown, 'w'}
[18523]  {:unknown, 'e'}
[18524]  {:unknown, 'a'}
[18525]  {:unknown, 'r'}
[18526]  {:unknown, '.'}
[18527]  {:unknown, ' '}
[18528]  {:unknown, ' '}
[18529]  {:unknown, 'I'}
[18530]  {:unknown, 't'}
[18531]  {:unknown, ' '}
[18532]  {:unknown, 'i'}
[18533]  {:unknown, 's'}
[18534]  {:unknown, ' '}
[18535]  {:unknown, 'l'}
[18536]  {:unknown, 'i'}
[18537]  {:unknown, 't'}
[18538]  {:unknown, ' '}
[18539]  {:unknown, 'a'}
[18540]  {:unknown, 'n'}
[18541]  {:unknown, 'd'}
[18542]  {:unknown, ' '}
[18543]  {:unknown, 'p'}
[18544]  {:unknown, 'r'}
[18545]  {:unknown, 'o'}
[18546]  {:unknown, 'd'}
[18547]  {:unknown, 'u'}
[18548]  {:unknown, 'c'}
[18549]  {:unknown, 'i'}
[18550]  {:unknown, 'n'}
[18551]  {:unknown, 'g'}
[18552]  {:unknown, ' '}
[18553]  {:unknown, 'a'}
[18554]  {:unknown, ' '}
[18555]  {:unknown, 'b'}
[18556]  {:unknown, 'r'}
[18557]  {:unknown, 'i'}
[18558]  {:unknown, 'g'}
[18559]  {:unknown, 'h'}
[18560]  {:unknown, 't'}
[18561]  {:unknown, ' '}
[18562]  {:unknown, 'l'}
[18563]  {:unknown, 'i'}
[18564]  {:unknown, 'g'}
[18565]  {:unknown, 'h'}
[18566]  {:unknown, 't'}
[18567]  {:unknown, '.'}
[18568]  {:pop, {:value, 99}}
[18570]  {:unknown, 'a'}
[18571]  {:unknown, 'n'}
[18572]  {:unknown, '-'}
[18573]  {:unknown, 'T'}
[18574]  {:unknown, 'h'}
[18575]  {:unknown, 'i'}
[18576]  {:unknown, 's'}
[18577]  {:unknown, ' '}
[18578]  {:unknown, 'c'}
[18579]  {:unknown, 'a'}
[18580]  {:unknown, 'n'}
[18581]  {:unknown, ' '}
[18582]  {:unknown, 'i'}
[18583]  {:unknown, 's'}
[18584]  {:unknown, ' '}
[18585]  {:unknown, 'f'}
[18586]  {:unknown, 'u'}
[18587]  {:unknown, 'l'}
[18588]  {:unknown, 'l'}
[18589]  {:unknown, ' '}
[18590]  {:unknown, 'o'}
[18591]  {:unknown, 'f'}
[18592]  {:unknown, ' '}
[18593]  {:unknown, 'h'}
[18594]  {:unknown, 'i'}
[18595]  {:unknown, 'g'}
[18596]  {:unknown, 'h'}
[18597]  {:unknown, '-'}
[18598]  {:unknown, 'q'}
[18599]  {:unknown, 'u'}
[18600]  {:unknown, 'a'}
[18601]  {:unknown, 'l'}
[18602]  {:unknown, 'i'}
[18603]  {:unknown, 't'}				# " "
[18604]  {:unknown, 'y'}
[18605]  {:unknown, ' '}
[18606]  {:unknown, 'l'}
[18607]  {:unknown, 'a'}
[18608]  {:unknown, 'n'}
[18609]  {:unknown, 't'}
[18610]  {:unknown, 'e'}
[18611]  {:unknown, 'r'}
[18612]  {:unknown, 'n'}
[18613]  {:unknown, ' '}
[18614]  {:unknown, 'o'}
[18615]  {:unknown, 'i'}
[18616]  {:unknown, 'l'}
[18617]  {:unknown, '.'}
[18618]  {:jf, {:value, 114}, {:value, 101}}
[18621]  {:unknown, 'd'}
[18622]  {:unknown, ' '}
[18623]  {:unknown, 'c'}
[18624]  {:unknown, 'o'}
[18625]  {:unknown, 'i'}
[18626]  {:unknown, 'n'}
[18627]  {:unknown, '?'}
[18628]  {:unknown, 'T'}
[18629]  {:unknown, 'h'}
[18630]  {:unknown, 'i'}
[18631]  {:unknown, 's'}
[18632]  {:unknown, ' '}
[18633]  {:unknown, 'c'}
[18634]  {:unknown, 'o'}
[18635]  {:unknown, 'i'}
[18636]  {:unknown, 'n'}
[18637]  {:unknown, ' '}
[18638]  {:unknown, 'i'}
[18639]  {:unknown, 's'}
[18640]  {:unknown, ' '}
[18641]  {:unknown, 'm'}
[18642]  {:unknown, 'a'}
[18643]  {:unknown, 'd'}
[18644]  {:unknown, 'e'}
[18645]  {:unknown, ' '}
[18646]  {:unknown, 'o'}
[18647]  {:unknown, 'f'}
[18648]  {:unknown, ' '}
[18649]  {:unknown, 'a'}
[18650]  {:unknown, ' '}
[18651]  {:unknown, 'r'}
[18652]  {:unknown, 'e'}
[18653]  {:unknown, 'd'}
[18654]  {:unknown, ' '}
[18655]  {:unknown, 'm'}
[18656]  {:unknown, 'e'}
[18657]  {:unknown, 't'}
[18658]  {:unknown, 'a'}
[18659]  {:unknown, 'l'}
[18660]  {:unknown, '.'}
[18661]  {:unknown, ' '}
[18662]  {:unknown, ' '}
[18663]  {:unknown, 'I'}
[18664]  {:unknown, 't'}
[18665]  {:unknown, ' '}
[18666]  {:unknown, 'h'}
[18667]  {:unknown, 'a'}
[18668]  {:unknown, 's'}
[18669]  {:unknown, ' '}
[18670]  {:unknown, 't'}
[18671]  {:unknown, 'w'}
[18672]  {:unknown, 'o'}
[18673]  {:unknown, ' '}
[18674]  {:unknown, 'd'}
[18675]  {:unknown, 'o'}
[18676]  {:unknown, 't'}
[18677]  {:unknown, 's'}
[18678]  {:unknown, ' '}
[18679]  {:unknown, 'o'}
[18680]  {:unknown, 'n'}
[18681]  {:unknown, ' '}
[18682]  {:unknown, 'o'}
[18683]  {:unknown, 'n'}
[18684]  {:unknown, 'e'}
[18685]  {:unknown, ' '}
[18686]  {:unknown, 's'}
[18687]  {:unknown, 'i'}
[18688]  {:unknown, 'd'}
[18689]  {:unknown, 'e'}
[18690]  {:unknown, '.'}
[18691]  {:or, {:value, 99}, {:value, 111}, {:value, 114}}
[18695]  {:unknown, 'r'}
[18696]  {:unknown, 'o'}
[18697]  {:unknown, 'd'}
[18698]  {:unknown, 'e'}
[18699]  {:unknown, 'd'}
[18700]  {:unknown, ' '}
[18701]  {:unknown, 'c'}
[18702]  {:unknown, 'o'}
[18703]  {:unknown, 'i'}
[18704]  {:unknown, 'n'}
[18705]  {:unknown, '?'}
[18706]  {:unknown, 'T'}
[18707]  {:unknown, 'h'}
[18708]  {:unknown, 'i'}
[18709]  {:unknown, 's'}
[18710]  {:unknown, ' '}
[18711]  {:unknown, 'c'}
[18712]  {:unknown, 'o'}
[18713]  {:unknown, 'i'}
[18714]  {:unknown, 'n'}
[18715]  {:unknown, ' '}
[18716]  {:unknown, 'i'}
[18717]  {:unknown, 's'}
[18718]  {:unknown, ' '}
[18719]  {:unknown, 's'}
[18720]  {:unknown, 'o'}
[18721]  {:unknown, 'm'}
[18722]  {:unknown, 'e'}
[18723]  {:unknown, 'w'}
[18724]  {:unknown, 'h'}
[18725]  {:unknown, 'a'}
[18726]  {:unknown, 't'}
[18727]  {:unknown, ' '}
[18728]  {:unknown, 'c'}
[18729]  {:unknown, 'o'}
[18730]  {:unknown, 'r'}
[18731]  {:unknown, 'r'}
[18732]  {:unknown, 'o'}
[18733]  {:unknown, 'd'}
[18734]  {:unknown, 'e'}
[18735]  {:unknown, 'd'}
[18736]  {:unknown, '.'}
[18737]  {:unknown, ' '}
[18738]  {:unknown, ' '}
[18739]  {:unknown, 'I'}
[18740]  {:unknown, 't'}
[18741]  {:unknown, ' '}
[18742]  {:unknown, 'h'}
[18743]  {:unknown, 'a'}
[18744]  {:unknown, 's'}
[18745]  {:unknown, ' '}
[18746]  {:unknown, 'a'}
[18747]  {:unknown, ' '}
[18748]  {:unknown, 't'}
[18749]  {:unknown, 'r'}
[18750]  {:unknown, 'i'}
[18751]  {:unknown, 'a'}
[18752]  {:unknown, 'n'}
[18753]  {:unknown, 'g'}
[18754]  {:unknown, 'l'}
[18755]  {:unknown, 'e'}
[18756]  {:unknown, ' '}
[18757]  {:unknown, 'o'}
[18758]  {:unknown, 'n'}
[18759]  {:unknown, ' '}
[18760]  {:unknown, 'o'}
[18761]  {:unknown, 'n'}
[18762]  {:unknown, 'e'}
[18763]  {:unknown, ' '}
[18764]  {:unknown, 's'}
[18765]  {:unknown, 'i'}
[18766]  {:unknown, 'd'}
[18767]  {:unknown, 'e'}
[18768]  {:unknown, '.'}
[18769]  {:mult, {:value, 115}, {:value, 104}, {:value, 105}}
[18773]  {:unknown, 'n'}
[18774]  {:unknown, 'y'}
[18775]  {:unknown, ' '}
[18776]  {:unknown, 'c'}
[18777]  {:unknown, 'o'}
[18778]  {:unknown, 'i'}
[18779]  {:unknown, 'n'}
[18780]  {:unknown, 'G'}
[18781]  {:unknown, 'T'}
[18782]  {:unknown, 'h'}
[18783]  {:unknown, 'i'}
[18784]  {:unknown, 's'}
[18785]  {:unknown, ' '}
[18786]  {:unknown, 'c'}
[18787]  {:unknown, 'o'}
[18788]  {:unknown, 'i'}
[18789]  {:unknown, 'n'}
[18790]  {:unknown, ' '}
[18791]  {:unknown, 'i'}
[18792]  {:unknown, 's'}
[18793]  {:unknown, ' '}
[18794]  {:unknown, 's'}
[18795]  {:unknown, 'o'}
[18796]  {:unknown, 'm'}
[18797]  {:unknown, 'e'}
[18798]  {:unknown, 'h'}
[18799]  {:unknown, 'o'}
[18800]  {:unknown, 'w'}
[18801]  {:unknown, ' '}
[18802]  {:unknown, 's'}
[18803]  {:unknown, 't'}
[18804]  {:unknown, 'i'}
[18805]  {:unknown, 'l'}
[18806]  {:unknown, 'l'}
[18807]  {:unknown, ' '}
[18808]  {:unknown, 'q'}
[18809]  {:unknown, 'u'}
[18810]  {:unknown, 'i'}
[18811]  {:unknown, 't'}
[18812]  {:unknown, 'e'}
[18813]  {:unknown, ' '}
[18814]  {:unknown, 's'}
[18815]  {:unknown, 'h'}
[18816]  {:unknown, 'i'}
[18817]  {:unknown, 'n'}
[18818]  {:unknown, 'y'}
[18819]  {:unknown, '.'}
[18820]  {:unknown, ' '}
[18821]  {:unknown, ' '}
[18822]  {:unknown, 'I'}
[18823]  {:unknown, 't'}
[18824]  {:unknown, ' '}
[18825]  {:unknown, 'h'}
[18826]  {:unknown, 'a'}
[18827]  {:unknown, 's'}
[18828]  {:unknown, ' '}
[18829]  {:unknown, 'a'}
[18830]  {:unknown, ' '}
[18831]  {:unknown, 'p'}
[18832]  {:unknown, 'e'}
[18833]  {:unknown, 'n'}
[18834]  {:unknown, 't'}
[18835]  {:unknown, 'a'}
[18836]  {:unknown, 'g'}
[18837]  {:unknown, 'o'}
[18838]  {:unknown, 'n'}
[18839]  {:unknown, ' '}
[18840]  {:unknown, 'o'}
[18841]  {:unknown, 'n'}
[18842]  {:unknown, ' '}
[18843]  {:unknown, 'o'}
[18844]  {:unknown, 'n'}
[18845]  {:unknown, 'e'}
[18846]  {:unknown, ' '}
[18847]  {:unknown, 's'}
[18848]  {:unknown, 'i'}
[18849]  {:unknown, 'd'}
[18850]  {:unknown, 'e'}
[18851]  {:unknown, '.'}
[18852]  {:and, {:value, 99}, {:value, 111}, {:value, 110}}
[18856]  {:unknown, 'c'}
[18857]  {:unknown, 'a'}
[18858]  {:unknown, 'v'}
[18859]  {:unknown, 'e'}
[18860]  {:unknown, ' '}
[18861]  {:unknown, 'c'}
[18862]  {:unknown, 'o'}
[18863]  {:unknown, 'i'}
[18864]  {:unknown, 'n'}
[18865]  {:unknown, 'W'}
[18866]  {:unknown, 'T'}
[18867]  {:unknown, 'h'}
[18868]  {:unknown, 'i'}
[18869]  {:unknown, 's'}
[18870]  {:unknown, ' '}
[18871]  {:unknown, 'c'}
[18872]  {:unknown, 'o'}
[18873]  {:unknown, 'i'}
[18874]  {:unknown, 'n'}
[18875]  {:unknown, ' '}
[18876]  {:unknown, 'i'}
[18877]  {:unknown, 's'}
[18878]  {:unknown, ' '}
[18879]  {:unknown, 's'}
[18880]  {:unknown, 'l'}
[18881]  {:unknown, 'i'}
[18882]  {:unknown, 'g'}
[18883]  {:unknown, 'h'}
[18884]  {:unknown, 't'}
[18885]  {:unknown, 'l'}
[18886]  {:unknown, 'y'}
[18887]  {:unknown, ' '}
[18888]  {:unknown, 'r'}
[18889]  {:unknown, 'o'}
[18890]  {:unknown, 'u'}
[18891]  {:unknown, 'n'}
[18892]  {:unknown, 'd'}
[18893]  {:unknown, 'e'}
[18894]  {:unknown, 'd'}
[18895]  {:unknown, ','}
[18896]  {:unknown, ' '}
[18897]  {:unknown, 'a'}
[18898]  {:unknown, 'l'}
[18899]  {:unknown, 'm'}
[18900]  {:unknown, 'o'}
[18901]  {:unknown, 's'}
[18902]  {:unknown, 't'}
[18903]  {:unknown, ' '}
[18904]  {:unknown, 'l'}
[18905]  {:unknown, 'i'}
[18906]  {:unknown, 'k'}
[18907]  {:unknown, 'e'}
[18908]  {:unknown, ' '}
[18909]  {:unknown, 'a'}
[18910]  {:unknown, ' '}
[18911]  {:unknown, 't'}
[18912]  {:unknown, 'i'}
[18913]  {:unknown, 'n'}
[18914]  {:unknown, 'y'}
[18915]  {:unknown, ' '}
[18916]  {:unknown, 'b'}
[18917]  {:unknown, 'o'}
[18918]  {:unknown, 'w'}
[18919]  {:unknown, 'l'}
[18920]  {:unknown, '.'}
[18921]  {:unknown, ' '}
[18922]  {:unknown, ' '}
[18923]  {:unknown, 'I'}
[18924]  {:unknown, 't'}
[18925]  {:unknown, ' '}
[18926]  {:unknown, 'h'}
[18927]  {:unknown, 'a'}
[18928]  {:unknown, 's'}
[18929]  {:unknown, ' '}
[18930]  {:unknown, 's'}
[18931]  {:unknown, 'e'}
[18932]  {:unknown, 'v'}
[18933]  {:unknown, 'e'}
[18934]  {:unknown, 'n'}
[18935]  {:unknown, ' '}
[18936]  {:unknown, 'd'}
[18937]  {:unknown, 'o'}
[18938]  {:unknown, 't'}
[18939]  {:unknown, 's'}
[18940]  {:unknown, ' '}
[18941]  {:unknown, 'o'}
[18942]  {:unknown, 'n'}
[18943]  {:unknown, ' '}
[18944]  {:unknown, 'o'}
[18945]  {:unknown, 'n'}
[18946]  {:unknown, 'e'}
[18947]  {:unknown, ' '}
[18948]  {:unknown, 's'}
[18949]  {:unknown, 'i'}
[18950]  {:unknown, 'd'}
[18951]  {:unknown, 'e'}
[18952]  {:unknown, '.'}
[18953]  {:add, {:value, 98}, {:value, 108}, {:value, 117}}
[18957]  {:unknown, 'e'}
[18958]  {:unknown, ' '}
[18959]  {:unknown, 'c'}
[18960]  {:unknown, 'o'}
[18961]  {:unknown, 'i'}
[18962]  {:unknown, 'n'}
[18963]  {:unknown, 'A'}
[18964]  {:unknown, 'T'}
[18965]  {:unknown, 'h'}
[18966]  {:unknown, 'i'}
[18967]  {:unknown, 's'}
[18968]  {:unknown, ' '}
[18969]  {:unknown, 'c'}
[18970]  {:unknown, 'o'}
[18971]  {:unknown, 'i'}
[18972]  {:unknown, 'n'}
[18973]  {:unknown, ' '}
[18974]  {:unknown, 'i'}
[18975]  {:unknown, 's'}
[18976]  {:unknown, ' '}
[18977]  {:unknown, 'm'}
[18978]  {:unknown, 'a'}
[18979]  {:unknown, 'd'}
[18980]  {:unknown, 'e'}
[18981]  {:unknown, ' '}
[18982]  {:unknown, 'o'}
[18983]  {:unknown, 'f'}
[18984]  {:unknown, ' '}
[18985]  {:unknown, 'a'}
[18986]  {:unknown, ' '}
[18987]  {:unknown, 'b'}
[18988]  {:unknown, 'l'}
[18989]  {:unknown, 'u'}
[18990]  {:unknown, 'e'}
[18991]  {:unknown, ' '}
[18992]  {:unknown, 'm'}
[18993]  {:unknown, 'e'}
[18994]  {:unknown, 't'}
[18995]  {:unknown, 'a'}
[18996]  {:unknown, 'l'}
[18997]  {:unknown, '.'}
[18998]  {:unknown, ' '}
[18999]  {:unknown, ' '}
[19000]  {:unknown, 'I'}
[19001]  {:unknown, 't'}
[19002]  {:unknown, ' '}
[19003]  {:unknown, 'h'}
[19004]  {:unknown, 'a'}
[19005]  {:unknown, 's'}
[19006]  {:unknown, ' '}
[19007]  {:unknown, 'n'}
[19008]  {:unknown, 'i'}
[19009]  {:unknown, 'n'}
[19010]  {:unknown, 'e'}
[19011]  {:unknown, ' '}
[19012]  {:unknown, 'd'}
[19013]  {:unknown, 'o'}
[19014]  {:unknown, 't'}
[19015]  {:unknown, 's'}
[19016]  {:unknown, ' '}
[19017]  {:unknown, 'o'}
[19018]  {:unknown, 'n'}
[19019]  {:unknown, ' '}
[19020]  {:unknown, 'o'}
[19021]  {:unknown, 'n'}
[19022]  {:unknown, 'e'}
[19023]  {:unknown, ' '}
[19024]  {:unknown, 's'}
[19025]  {:unknown, 'i'}
[19026]  {:unknown, 'd'}
[19027]  {:unknown, 'e'}
[19028]  {:unknown, '.'}
[19029]  {:mult, {:value, 116}, {:value, 101}, {:value, 108}}
[19033]  {:unknown, 'e'}
[19034]  {:unknown, 'p'}
[19035]  {:unknown, 'o'}
[19036]  {:unknown, 'r'}
[19037]  {:unknown, 't'}
[19038]  {:unknown, 'e'}
[19039]  {:unknown, 'r'}
[19040]  {:unknown, 'H'}
[19041]  {:unknown, 'T'}
[19042]  {:unknown, 'h'}
[19043]  {:unknown, 'i'}
[19044]  {:unknown, 's'}
[19045]  {:unknown, ' '}
[19046]  {:unknown, 's'}
[19047]  {:unknown, 'm'}
[19048]  {:unknown, 'a'}
[19049]  {:unknown, 'l'}
[19050]  {:unknown, 'l'}
[19051]  {:unknown, ' '}
[19052]  {:unknown, 'd'}
[19053]  {:unknown, 'e'}
[19054]  {:unknown, 'v'}
[19055]  {:unknown, 'i'}
[19056]  {:unknown, 'c'}
[19057]  {:unknown, 'e'}
[19058]  {:unknown, ' '}
[19059]  {:unknown, 'h'}
[19060]  {:unknown, 'a'}
[19061]  {:unknown, 's'}
[19062]  {:unknown, ' '}
[19063]  {:unknown, 'a'}
[19064]  {:unknown, ' '}
[19065]  {:unknown, 'b'}
[19066]  {:unknown, 'u'}
[19067]  {:unknown, 't'}
[19068]  {:unknown, 't'}
[19069]  {:unknown, 'o'}
[19070]  {:unknown, 'n'}
[19071]  {:unknown, ' '}
[19072]  {:unknown, 'o'}
[19073]  {:unknown, 'n'}
[19074]  {:unknown, ' '}
[19075]  {:unknown, 'i'}
[19076]  {:unknown, 't'}
[19077]  {:unknown, ' '}
[19078]  {:unknown, 'a'}
[19079]  {:unknown, 'n'}
[19080]  {:unknown, 'd'}
[19081]  {:unknown, ' '}
[19082]  {:unknown, 'r'}
[19083]  {:unknown, 'e'}
[19084]  {:unknown, 'a'}
[19085]  {:unknown, 'd'}
[19086]  {:unknown, 's'}
[19087]  {:unknown, ' '}
[19088]  {:unknown, '"'}
[19089]  {:unknown, 't'}
[19090]  {:unknown, 'e'}
[19091]  {:unknown, 'l'}
[19092]  {:unknown, 'e'}
[19093]  {:unknown, 'p'}
[19094]  {:unknown, 'o'}
[19095]  {:unknown, 'r'}
[19096]  {:unknown, 't'}
[19097]  {:unknown, 'e'}
[19098]  {:unknown, 'r'}
[19099]  {:unknown, '"'}
[19100]  {:unknown, ' '}
[19101]  {:unknown, 'o'}
[19102]  {:unknown, 'n'}
[19103]  {:unknown, ' '}
[19104]  {:unknown, 't'}
[19105]  {:unknown, 'h'}
[19106]  {:unknown, 'e'}
[19107]  {:unknown, ' '}
[19108]  {:unknown, 's'}
[19109]  {:unknown, 'i'}
[19110]  {:unknown, 'd'}
[19111]  {:unknown, 'e'}
[19112]  {:unknown, '.'}
[19113]  {:or, {:value, 98}, {:value, 117}, {:value, 115}}
[19117]  {:unknown, 'i'}
[19118]  {:unknown, 'n'}
[19119]  {:unknown, 'e'}
[19120]  {:unknown, 's'}
[19121]  {:unknown, 's'}
[19122]  {:unknown, ' '}
[19123]  {:unknown, 'c'}
[19124]  {:unknown, 'a'}
[19125]  {:unknown, 'r'}
[19126]  {:unknown, 'd'}
[19127]  {:unknown, '@'}
[19128]  {:unknown, 'T'}
[19129]  {:unknown, 'h'}
[19130]  {:unknown, 'i'}
[19131]  {:unknown, 's'}
[19132]  {:unknown, ' '}
[19133]  {:unknown, 'b'}
[19134]  {:unknown, 'u'}
[19135]  {:unknown, 's'}
[19136]  {:unknown, 'i'}
[19137]  {:unknown, 'n'}
[19138]  {:unknown, 'e'}
[19139]  {:unknown, 's'}
[19140]  {:unknown, 's'}
[19141]  {:unknown, ' '}
[19142]  {:unknown, 'c'}
[19143]  {:unknown, 'a'}
[19144]  {:unknown, 'r'}
[19145]  {:unknown, 'd'}
[19146]  {:unknown, ' '}
[19147]  {:unknown, 'h'}
[19148]  {:unknown, 'a'}
[19149]  {:unknown, 's'}
[19150]  {:unknown, ' '}
[19151]  {:unknown, '"'}
[19152]  {:unknown, 's'}
[19153]  {:unknown, 'y'}
[19154]  {:unknown, 'n'}
[19155]  {:unknown, 'a'}
[19156]  {:unknown, 'c'}
[19157]  {:unknown, 'o'}
[19158]  {:unknown, 'r'}
[19159]  {:unknown, '.'}
[19160]  {:unknown, 'c'}
[19161]  {:unknown, 'o'}
[19162]  {:unknown, 'm'}
[19163]  {:unknown, '"'}
[19164]  {:unknown, ' '}
[19165]  {:unknown, 'p'}
[19166]  {:unknown, 'r'}
[19167]  {:unknown, 'i'}
[19168]  {:unknown, 'n'}
[19169]  {:unknown, 't'}
[19170]  {:unknown, 'e'}
[19171]  {:unknown, 'd'}
[19172]  {:unknown, ' '}
[19173]  {:unknown, 'i'}
[19174]  {:unknown, 'n'}
[19175]  {:unknown, ' '}
[19176]  {:unknown, 'r'}
[19177]  {:unknown, 'e'}
[19178]  {:unknown, 'd'}
[19179]  {:unknown, ' '}
[19180]  {:unknown, 'o'}
[19181]  {:unknown, 'n'}
[19182]  {:unknown, ' '}
[19183]  {:unknown, 'o'}
[19184]  {:unknown, 'n'}
[19185]  {:unknown, 'e'}
[19186]  {:unknown, ' '}
[19187]  {:unknown, 's'}
[19188]  {:unknown, 'i'}
[19189]  {:unknown, 'd'}
[19190]  {:unknown, 'e'}
[19191]  {:unknown, '.'}
[19192]  {:pop, {:value, 111}}
[19194]  {:unknown, 'r'}
[19195]  {:unknown, 'b'}
[19196]  {:unknown, '='}
[19197]  {:unknown, 'T'}
[19198]  {:unknown, 'h'}
[19199]  {:unknown, 'i'}
[19200]  {:unknown, 's'}
[19201]  {:unknown, ' '}
[19202]  {:unknown, 'i'}
[19203]  {:unknown, 's'}
[19204]  {:unknown, ' '}
[19205]  {:unknown, 'a'}
[19206]  {:unknown, ' '}
[19207]  {:unknown, 'c'}
[19208]  {:unknown, 'l'}
[19209]  {:unknown, 'e'}
[19210]  {:unknown, 'a'}
[19211]  {:unknown, 'r'}
[19212]  {:unknown, ' '}
[19213]  {:unknown, 'g'}
[19214]  {:unknown, 'l'}
[19215]  {:unknown, 'a'}
[19216]  {:unknown, 's'}
[19217]  {:unknown, 's'}
[19218]  {:unknown, ' '}
[19219]  {:unknown, 's'}
[19220]  {:unknown, 'p'}
[19221]  {:unknown, 'h'}
[19222]  {:unknown, 'e'}
[19223]  {:unknown, 'r'}
[19224]  {:unknown, 'e'}
[19225]  {:unknown, ' '}
[19226]  {:unknown, 'a'}
[19227]  {:unknown, 'b'}
[19228]  {:unknown, 'o'}
[19229]  {:unknown, 'u'}
[19230]  {:unknown, 't'}
[19231]  {:unknown, ' '}
[19232]  {:unknown, 't'}
[19233]  {:unknown, 'h'}
[19234]  {:unknown, 'e'}
[19235]  {:unknown, ' '}
[19236]  {:unknown, 's'}
[19237]  {:unknown, 'i'}
[19238]  {:unknown, 'z'}
[19239]  {:unknown, 'e'}
[19240]  {:unknown, ' '}
[19241]  {:unknown, 'o'}
[19242]  {:unknown, 'f'}
[19243]  {:unknown, ' '}
[19244]  {:unknown, 'a'}
[19245]  {:unknown, ' '}
[19246]  {:unknown, 't'}
[19247]  {:unknown, 'e'}
[19248]  {:unknown, 'n'}
[19249]  {:unknown, 'n'}
[19250]  {:unknown, 'i'}
[19251]  {:unknown, 's'}
[19252]  {:unknown, ' '}
[19253]  {:unknown, 'b'}
[19254]  {:unknown, 'a'}
[19255]  {:unknown, 'l'}
[19256]  {:unknown, 'l'}
[19257]  {:unknown, '.'}
[19258]  {:jmp, {:value, 109}}
[19260]  {:unknown, 'i'}
[19261]  {:unknown, 'r'}
[19262]  {:unknown, 'r'}
[19263]  {:unknown, 'o'}
[19264]  {:unknown, 'r'}
[19265]  {:unknown, 'v'}
[19266]  {:unknown, 'T'}
[19267]  {:unknown, 'h'}
[19268]  {:unknown, 'i'}
[19269]  {:unknown, 's'}
[19270]  {:unknown, ' '}
[19271]  {:unknown, 'i'}
[19272]  {:unknown, 's'}
[19273]  {:unknown, ' '}
[19274]  {:unknown, 'a'}
[19275]  {:unknown, ' '}
[19276]  {:unknown, 'r'}
[19277]  {:unknown, 'a'}
[19278]  {:unknown, 't'}
[19279]  {:unknown, 'h'}
[19280]  {:unknown, 'e'}
[19281]  {:unknown, 'r'}
[19282]  {:unknown, ' '}
[19283]  {:unknown, 'm'}
[19284]  {:unknown, 'u'}
[19285]  {:unknown, 'n'}
[19286]  {:unknown, 'd'}
[19287]  {:unknown, 'a'}
[19288]  {:unknown, 'n'}
[19289]  {:unknown, 'e'}
[19290]  {:unknown, ' '}
[19291]  {:unknown, 'h'}
[19292]  {:unknown, 'a'}
[19293]  {:unknown, 'n'}
[19294]  {:unknown, 'd'}
[19295]  {:unknown, '-'}
[19296]  {:unknown, 'h'}
[19297]  {:unknown, 'e'}
[19298]  {:unknown, 'l'}
[19299]  {:unknown, 'd'}
[19300]  {:unknown, ' '}
[19301]  {:unknown, 'm'}
[19302]  {:unknown, 'i'}
[19303]  {:unknown, 'r'}
[19304]  {:unknown, 'r'}
[19305]  {:unknown, 'o'}
[19306]  {:unknown, 'r'}
[19307]  {:unknown, ' '}
[19308]  {:unknown, 'f'}
[19309]  {:unknown, 'r'}
[19310]  {:unknown, 'o'}
[19311]  {:unknown, 'm'}
[19312]  {:unknown, ' '}
[19313]  {:unknown, 't'}
[19314]  {:unknown, 'h'}
[19315]  {:unknown, 'e'}
[19316]  {:unknown, ' '}
[19317]  {:unknown, 'o'}
[19318]  {:unknown, 't'}
[19319]  {:unknown, 'h'}
[19320]  {:unknown, 'e'}
[19321]  {:unknown, 'r'}
[19322]  {:unknown, 'w'}
[19323]  {:unknown, 'i'}
[19324]  {:unknown, 's'}
[19325]  {:unknown, 'e'}
[19326]  {:unknown, ' '}
[19327]  {:unknown, 'm'}
[19328]  {:unknown, 'a'}
[19329]  {:unknown, 'g'}
[19330]  {:unknown, 'n'}
[19331]  {:unknown, 'i'}
[19332]  {:unknown, 'f'}
[19333]  {:unknown, 'i'}
[19334]  {:unknown, 'c'}
[19335]  {:unknown, 'e'}
[19336]  {:unknown, 'n'}
[19337]  {:unknown, 't'}
[19338]  {:unknown, ' '}
[19339]  {:unknown, 'v'}
[19340]  {:unknown, 'a'}
[19341]  {:unknown, 'u'}
[19342]  {:unknown, 'l'}
[19343]  {:unknown, 't'}
[19344]  {:unknown, ' '}
[19345]  {:unknown, 'r'}
[19346]  {:unknown, 'o'}
[19347]  {:unknown, 'o'}
[19348]  {:unknown, 'm'}
[19349]  {:unknown, '.'}
[19350]  {:unknown, ' '}
[19351]  {:unknown, ' '}
[19352]  {:unknown, 'W'}
[19353]  {:unknown, 'h'}
[19354]  {:unknown, 'a'}
[19355]  {:unknown, 't'}
[19356]  {:unknown, ' '}
[19357]  {:unknown, 'U'}
[19358]  {:unknown, 'S'}
[19359]  {:unknown, 'E'}
[19360]  {:unknown, ' '}
[19361]  {:unknown, 'c'}
[19362]  {:unknown, 'o'}
[19363]  {:unknown, 'u'}
[19364]  {:unknown, 'l'}
[19365]  {:unknown, 'd'}
[19366]  {:unknown, ' '}
[19367]  {:unknown, 'i'}
[19368]  {:unknown, 't'}
[19369]  {:unknown, ' '}
[19370]  {:unknown, 'p'}
[19371]  {:unknown, 'o'}
[19372]  {:unknown, 's'}
[19373]  {:unknown, 's'}
[19374]  {:unknown, 'i'}
[19375]  {:unknown, 'b'}
[19376]  {:unknown, 'l'}
[19377]  {:unknown, 'y'}
[19378]  {:unknown, ' '}
[19379]  {:unknown, 'h'}
[19380]  {:unknown, 'a'}
[19381]  {:unknown, 'v'}
[19382]  {:unknown, 'e'}
[19383]  {:unknown, '?'}
[19384]  {:and, {:value, 115}, {:value, 116}, {:value, 114}}
[19388]  {:unknown, 'a'}
[19389]  {:unknown, 'n'}
[19390]  {:unknown, 'g'}
[19391]  {:unknown, 'e'}
[19392]  {:unknown, ' '}
[19393]  {:unknown, 'b'}
[19394]  {:unknown, 'o'}
[19395]  {:unknown, 'o'}
[19396]  {:unknown, 'k'}
[19397]  {:unknown, [2695]}
[19398]  {:unknown, 'T'}
[19399]  {:unknown, 'h'}
[19400]  {:unknown, 'e'}
[19401]  {:unknown, ' '}
[19402]  {:unknown, 'c'}
[19403]  {:unknown, 'o'}
[19404]  {:unknown, 'v'}
[19405]  {:unknown, 'e'}
[19406]  {:unknown, 'r'}
[19407]  {:unknown, ' '}
[19408]  {:unknown, 'o'}
[19409]  {:unknown, 'f'}
[19410]  {:unknown, ' '}
[19411]  {:unknown, 't'}
[19412]  {:unknown, 'h'}
[19413]  {:unknown, 'i'}
[19414]  {:unknown, 's'}
[19415]  {:unknown, ' '}
[19416]  {:unknown, 'b'}
[19417]  {:unknown, 'o'}
[19418]  {:unknown, 'o'}
[19419]  {:unknown, 'k'}
[19420]  {:unknown, ' '}
[19421]  {:unknown, 's'}
[19422]  {:unknown, 'u'}
[19423]  {:unknown, 'b'}
[19424]  {:unknown, 't'}
[19425]  {:unknown, 'l'}
[19426]  {:unknown, 'y'}
[19427]  {:unknown, ' '}
[19428]  {:unknown, 's'}
[19429]  {:unknown, 'w'}
[19430]  {:unknown, 'i'}
[19431]  {:unknown, 'r'}
[19432]  {:unknown, 'l'}
[19433]  {:unknown, 's'}
[19434]  {:unknown, ' '}
[19435]  {:unknown, 'w'}
[19436]  {:unknown, 'i'}
[19437]  {:unknown, 't'}
[19438]  {:unknown, 'h'}
[19439]  {:unknown, ' '}
[19440]  {:unknown, 'c'}
[19441]  {:unknown, 'o'}
[19442]  {:unknown, 'l'}
[19443]  {:unknown, 'o'}
[19444]  {:unknown, 'r'}
[19445]  {:unknown, 's'}
[19446]  {:unknown, '.'}
[19447]  {:unknown, ' '}
[19448]  {:unknown, ' '}
[19449]  {:unknown, 'I'}
[19450]  {:unknown, 't'}
[19451]  {:unknown, ' '}
[19452]  {:unknown, 'i'}
[19453]  {:unknown, 's'}
[19454]  {:unknown, ' '}
[19455]  {:unknown, 't'}
[19456]  {:unknown, 'i'}
[19457]  {:unknown, 't'}
[19458]  {:unknown, 'l'}
[19459]  {:unknown, 'e'}
[19460]  {:unknown, 'd'}
[19461]  {:unknown, ' '}
[19462]  {:unknown, '"'}
[19463]  {:unknown, 'A'}
[19464]  {:unknown, ' '}
[19465]  {:unknown, 'B'}
[19466]  {:unknown, 'r'}
[19467]  {:unknown, 'i'}
[19468]  {:unknown, 'e'}
[19469]  {:unknown, 'f'}
[19470]  {:unknown, ' '}
[19471]  {:unknown, 'I'}
[19472]  {:unknown, 'n'}
[19473]  {:unknown, 't'}
[19474]  {:unknown, 'r'}
[19475]  {:unknown, 'o'}
[19476]  {:unknown, 'd'}
[19477]  {:unknown, 'u'}
[19478]  {:unknown, 'c'}
[19479]  {:unknown, 't'}
[19480]  {:unknown, 'i'}
[19481]  {:unknown, 'o'}
[19482]  {:unknown, 'n'}
[19483]  {:unknown, ' '}
[19484]  {:unknown, 't'}
[19485]  {:unknown, 'o'}
[19486]  {:unknown, ' '}
[19487]  {:unknown, 'I'}
[19488]  {:unknown, 'n'}
[19489]  {:unknown, 't'}
[19490]  {:unknown, 'e'}
[19491]  {:unknown, 'r'}
[19492]  {:unknown, 'd'}
[19493]  {:unknown, 'i'}
[19494]  {:unknown, 'm'}
[19495]  {:unknown, 'e'}
[19496]  {:unknown, 'n'}
[19497]  {:unknown, 's'}
[19498]  {:unknown, 'i'}
[19499]  {:unknown, 'o'}
[19500]  {:unknown, 'n'}
[19501]  {:unknown, 'a'}
[19502]  {:unknown, 'l'}
[19503]  {:unknown, ' '}
[19504]  {:unknown, 'P'}
[19505]  {:unknown, 'h'}
[19506]  {:unknown, 'y'}
[19507]  {:unknown, 's'}
[19508]  {:unknown, 'i'}
[19509]  {:unknown, 'c'}
[19510]  {:unknown, 's'}
[19511]  {:unknown, '"'}
[19512]  {:unknown, '.'}
[19513]  {:unknown, ' '}
[19514]  {:unknown, ' '}
[19515]  {:unknown, 'I'}
[19516]  {:unknown, 't'}
[19517]  {:unknown, ' '}
[19518]  {:unknown, 'r'}
[19519]  {:unknown, 'e'}
[19520]  {:unknown, 'a'}
[19521]  {:unknown, 'd'}
[19522]  {:unknown, 's'}
[19523]  {:unknown, ':'}
[19524]  {:mult, {:value, 10}, {:value, 82}, {:value, 101}}
[19528]  {:unknown, 'c'}
[19529]  {:unknown, 'e'}
[19530]  {:unknown, 'n'}
[19531]  {:unknown, 't'}
[19532]  {:unknown, ' '}
[19533]  {:unknown, 'a'}
[19534]  {:unknown, 'd'}
[19535]  {:unknown, 'v'}
[19536]  {:unknown, 'a'}
[19537]  {:unknown, 'n'}
[19538]  {:unknown, 'c'}
[19539]  {:unknown, 'e'}
[19540]  {:unknown, 's'}
[19541]  {:unknown, ' '}
[19542]  {:unknown, 'i'}
[19543]  {:unknown, 'n'}
[19544]  {:unknown, ' '}
[19545]  {:unknown, 'i'}
[19546]  {:unknown, 'n'}
[19547]  {:unknown, 't'}
[19548]  {:unknown, 'e'}
[19549]  {:unknown, 'r'}
[19550]  {:unknown, 'd'}
[19551]  {:unknown, 'i'}
[19552]  {:unknown, 'm'}
[19553]  {:unknown, 'e'}
[19554]  {:unknown, 'n'}
[19555]  {:unknown, 's'}
[19556]  {:unknown, 'i'}
[19557]  {:unknown, 'o'}
[19558]  {:unknown, 'n'}
[19559]  {:unknown, 'a'}
[19560]  {:unknown, 'l'}
[19561]  {:unknown, ' '}
[19562]  {:unknown, 'p'}
[19563]  {:unknown, 'h'}
[19564]  {:unknown, 'y'}
[19565]  {:unknown, 's'}
[19566]  {:unknown, 'i'}
[19567]  {:unknown, 'c'}
[19568]  {:unknown, 's'}
[19569]  {:unknown, ' '}
[19570]  {:unknown, 'h'}
[19571]  {:unknown, 'a'}
[19572]  {:unknown, 'v'}
[19573]  {:unknown, 'e'}
[19574]  {:unknown, ' '}
[19575]  {:unknown, 'p'}
[19576]  {:unknown, 'r'}
[19577]  {:unknown, 'o'}
[19578]  {:unknown, 'd'}
[19579]  {:unknown, 'u'}
[19580]  {:unknown, 'c'}
[19581]  {:unknown, 'e'}
[19582]  {:unknown, 'd'}
[19583]  {:unknown, ' '}
[19584]  {:unknown, 'f'}
[19585]  {:unknown, 'a'}
[19586]  {:unknown, 's'}
[19587]  {:unknown, 'c'}
[19588]  {:unknown, 'i'}
[19589]  {:unknown, 'n'}
[19590]  {:unknown, 'a'}
[19591]  {:unknown, 't'}
[19592]  {:unknown, 'i'}
[19593]  {:unknown, 'n'}
[19594]  {:unknown, 'g'}
[19595]  {:mult, {:value, 112}, {:value, 114}, {:value, 101}}
[19599]  {:unknown, 'd'}
[19600]  {:unknown, 'i'}
[19601]  {:unknown, 'c'}
[19602]  {:unknown, 't'}
[19603]  {:unknown, 'i'}
[19604]  {:unknown, 'o'}
[19605]  {:unknown, 'n'}
[19606]  {:unknown, 's'}
[19607]  {:unknown, ' '}
[19608]  {:unknown, 'a'}
[19609]  {:unknown, 'b'}
[19610]  {:unknown, 'o'}
[19611]  {:unknown, 'u'}
[19612]  {:unknown, 't'}
[19613]  {:unknown, ' '}
[19614]  {:unknown, 't'}
[19615]  {:unknown, 'h'}
[19616]  {:unknown, 'e'}
[19617]  {:unknown, ' '}
[19618]  {:unknown, 'f'}
[19619]  {:unknown, 'u'}
[19620]  {:unknown, 'n'}
[19621]  {:unknown, 'd'}
[19622]  {:unknown, 'a'}
[19623]  {:unknown, 'm'}
[19624]  {:unknown, 'e'}
[19625]  {:unknown, 'n'}
[19626]  {:unknown, 't'}
[19627]  {:unknown, 'a'}
[19628]  {:unknown, 'l'}
[19629]  {:unknown, 's'}
[19630]  {:unknown, ' '}
[19631]  {:unknown, 'o'}
[19632]  {:unknown, 'f'}
[19633]  {:unknown, ' '}
[19634]  {:unknown, 'o'}
[19635]  {:unknown, 'u'}
[19636]  {:unknown, 'r'}
[19637]  {:unknown, ' '}
[19638]  {:unknown, 'u'}
[19639]  {:unknown, 'n'}
[19640]  {:unknown, 'i'}
[19641]  {:unknown, 'v'}
[19642]  {:unknown, 'e'}
[19643]  {:unknown, 'r'}
[19644]  {:unknown, 's'}
[19645]  {:unknown, 'e'}
[19646]  {:unknown, '!'}
[19647]  {:unknown, ' '}
[19648]  {:unknown, ' '}
[19649]  {:unknown, 'F'}
[19650]  {:unknown, 'o'}
[19651]  {:unknown, 'r'}
[19652]  {:unknown, ' '}
[19653]  {:unknown, 'e'}
[19654]  {:unknown, 'x'}
[19655]  {:unknown, 'a'}
[19656]  {:unknown, 'm'}
[19657]  {:unknown, 'p'}
[19658]  {:unknown, 'l'}
[19659]  {:unknown, 'e'}
[19660]  {:unknown, ','}
[19661]  {:mult, {:value, 105}, {:value, 110}, {:value, 116}}
[19665]  {:unknown, 'e'}
[19666]  {:unknown, 'r'}
[19667]  {:unknown, 'd'}
[19668]  {:unknown, 'i'}
[19669]  {:unknown, 'm'}
[19670]  {:unknown, 'e'}
[19671]  {:unknown, 'n'}
[19672]  {:unknown, 's'}
[19673]  {:unknown, 'i'}
[19674]  {:unknown, 'o'}
[19675]  {:unknown, 'n'}
[19676]  {:unknown, 'a'}
[19677]  {:unknown, 'l'}
[19678]  {:unknown, ' '}
[19679]  {:unknown, 'p'}
[19680]  {:unknown, 'h'}
[19681]  {:unknown, 'y'}
[19682]  {:unknown, 's'}
[19683]  {:unknown, 'i'}
[19684]  {:unknown, 'c'}
[19685]  {:unknown, 's'}
[19686]  {:unknown, ' '}
[19687]  {:unknown, 's'}
[19688]  {:unknown, 'e'}
[19689]  {:unknown, 'e'}
[19690]  {:unknown, 'm'}
[19691]  {:unknown, 's'}
[19692]  {:unknown, ' '}
[19693]  {:unknown, 't'}
[19694]  {:unknown, 'o'}
[19695]  {:unknown, ' '}
[19696]  {:unknown, 'p'}
[19697]  {:unknown, 'r'}
[19698]  {:unknown, 'e'}
[19699]  {:unknown, 'd'}
[19700]  {:unknown, 'i'}
[19701]  {:unknown, 'c'}
[19702]  {:unknown, 't'}
[19703]  {:unknown, ' '}
[19704]  {:unknown, 't'}
[19705]  {:unknown, 'h'}
[19706]  {:unknown, 'a'}
[19707]  {:unknown, 't'}
[19708]  {:unknown, ' '}
[19709]  {:unknown, 't'}
[19710]  {:unknown, 'h'}
[19711]  {:unknown, 'e'}
[19712]  {:unknown, ' '}
[19713]  {:unknown, 'u'}
[19714]  {:unknown, 'n'}
[19715]  {:unknown, 'i'}
[19716]  {:unknown, 'v'}
[19717]  {:unknown, 'e'}
[19718]  {:unknown, 'r'}
[19719]  {:unknown, 's'}
[19720]  {:unknown, 'e'}
[19721]  {:unknown, ' '}
[19722]  {:unknown, 'i'}
[19723]  {:unknown, 's'}
[19724]  {:unknown, ','}
[19725]  {:unknown, ' '}
[19726]  {:unknown, 'a'}
[19727]  {:unknown, 't'}
[19728]  {:unknown, ' '}
[19729]  {:unknown, 'i'}
[19730]  {:unknown, 't'}
[19731]  {:unknown, 's'}
[19732]  {:unknown, ' '}
[19733]  {:unknown, 'r'}
[19734]  {:unknown, 'o'}
[19735]  {:unknown, 'o'}
[19736]  {:unknown, 't'}
[19737]  {:unknown, ','}
[19738]  {:unknown, ' '}
[19739]  {:unknown, 'a'}
[19740]  {:mult, {:value, 112}, {:value, 117}, {:value, 114}}
[19744]  {:unknown, 'e'}
[19745]  {:unknown, 'l'}
[19746]  {:unknown, 'y'}
[19747]  {:unknown, ' '}
[19748]  {:unknown, 'm'}
[19749]  {:unknown, 'a'}
[19750]  {:unknown, 't'}
[19751]  {:unknown, 'h'}
[19752]  {:unknown, 'e'}
[19753]  {:unknown, 'm'}
[19754]  {:unknown, 'a'}
[19755]  {:unknown, 't'}
[19756]  {:unknown, 'i'}
[19757]  {:unknown, 'c'}
[19758]  {:unknown, 'a'}
[19759]  {:unknown, 'l'}
[19760]  {:unknown, ' '}
[19761]  {:unknown, 'c'}
[19762]  {:unknown, 'o'}
[19763]  {:unknown, 'n'}
[19764]  {:unknown, 's'}
[19765]  {:unknown, 't'}
[19766]  {:unknown, 'r'}
[19767]  {:unknown, 'u'}
[19768]  {:unknown, 'c'}
[19769]  {:unknown, 't'}
[19770]  {:unknown, ','}
[19771]  {:unknown, ' '}
[19772]  {:unknown, 'a'}
[19773]  {:unknown, 'n'}
[19774]  {:unknown, 'd'}
[19775]  {:unknown, ' '}
[19776]  {:unknown, 't'}
[19777]  {:unknown, 'h'}
[19778]  {:unknown, 'a'}
[19779]  {:unknown, 't'}
[19780]  {:unknown, ' '}
[19781]  {:unknown, 'a'}
[19782]  {:unknown, 'l'}
[19783]  {:unknown, 'l'}
[19784]  {:unknown, ' '}
[19785]  {:unknown, 'e'}
[19786]  {:unknown, 'v'}
[19787]  {:unknown, 'e'}
[19788]  {:unknown, 'n'}
[19789]  {:unknown, 't'}
[19790]  {:unknown, 's'}
[19791]  {:unknown, ' '}
[19792]  {:unknown, 'a'}
[19793]  {:unknown, 'r'}
[19794]  {:unknown, 'e'}
[19795]  {:unknown, ' '}
[19796]  {:unknown, 'c'}
[19797]  {:unknown, 'a'}
[19798]  {:unknown, 'u'}
[19799]  {:unknown, 's'}
[19800]  {:unknown, 'e'}
[19801]  {:unknown, 'd'}
[19802]  {:unknown, ' '}
[19803]  {:unknown, 'b'}
[19804]  {:unknown, 'y'}
[19805]  {:unknown, ' '}
[19806]  {:unknown, 't'}
[19807]  {:unknown, 'h'}
[19808]  {:unknown, 'e'}
[19809]  {:mult, {:value, 105}, {:value, 110}, {:value, 116}}
[19813]  {:unknown, 'e'}
[19814]  {:unknown, 'r'}
[19815]  {:unknown, 'a'}
[19816]  {:unknown, 'c'}
[19817]  {:unknown, 't'}
[19818]  {:unknown, 'i'}
[19819]  {:unknown, 'o'}
[19820]  {:unknown, 'n'}
[19821]  {:unknown, 's'}
[19822]  {:unknown, ' '}
[19823]  {:unknown, 'b'}
[19824]  {:unknown, 'e'}
[19825]  {:unknown, 't'}
[19826]  {:unknown, 'w'}
[19827]  {:unknown, 'e'}
[19828]  {:unknown, 'e'}
[19829]  {:unknown, 'n'}
[19830]  {:unknown, ' '}
[19831]  {:unknown, 'e'}
[19832]  {:unknown, 'i'}
[19833]  {:unknown, 'g'}
[19834]  {:unknown, 'h'}
[19835]  {:unknown, 't'}
[19836]  {:unknown, ' '}
[19837]  {:unknown, 'p'}
[19838]  {:unknown, 'o'}
[19839]  {:unknown, 'c'}
[19840]  {:unknown, 'k'}
[19841]  {:unknown, 'e'}
[19842]  {:unknown, 't'}
[19843]  {:unknown, 's'}
[19844]  {:unknown, ' '}
[19845]  {:unknown, 'o'}
[19846]  {:unknown, 'f'}
[19847]  {:unknown, ' '}
[19848]  {:unknown, 'e'}
[19849]  {:unknown, 'n'}
[19850]  {:unknown, 'e'}
[19851]  {:unknown, 'r'}
[19852]  {:unknown, 'g'}
[19853]  {:unknown, 'y'}
[19854]  {:unknown, ' '}
[19855]  {:unknown, 'c'}
[19856]  {:unknown, 'a'}
[19857]  {:unknown, 'l'}
[19858]  {:unknown, 'l'}
[19859]  {:unknown, 'e'}
[19860]  {:unknown, 'd'}
[19861]  {:unknown, ' '}
[19862]  {:unknown, '"'}
[19863]  {:unknown, 'r'}
[19864]  {:unknown, 'e'}
[19865]  {:unknown, 'g'}
[19866]  {:unknown, 'i'}
[19867]  {:unknown, 's'}
[19868]  {:unknown, 't'}
[19869]  {:unknown, 'e'}
[19870]  {:unknown, 'r'}
[19871]  {:unknown, 's'}
[19872]  {:unknown, '"'}
[19873]  {:unknown, '.'}
[19874]  {:mult, {:value, 70}, {:value, 117}, {:value, 114}}
[19878]  {:unknown, 't'}
[19879]  {:unknown, 'h'}
[19880]  {:unknown, 'e'}
[19881]  {:unknown, 'r'}
[19882]  {:unknown, 'm'}
[19883]  {:unknown, 'o'}
[19884]  {:unknown, 'r'}
[19885]  {:unknown, 'e'}
[19886]  {:unknown, ','}
[19887]  {:unknown, ' '}
[19888]  {:unknown, 'i'}
[19889]  {:unknown, 't'}
[19890]  {:unknown, ' '}
[19891]  {:unknown, 's'}
[19892]  {:unknown, 'e'}
[19893]  {:unknown, 'e'}
[19894]  {:unknown, 'm'}
[19895]  {:unknown, 's'}
[19896]  {:unknown, ' '}
[19897]  {:unknown, 't'}
[19898]  {:unknown, 'h'}
[19899]  {:unknown, 'a'}
[19900]  {:unknown, 't'}
[19901]  {:unknown, ' '}
[19902]  {:unknown, 'w'}
[19903]  {:unknown, 'h'}
[19904]  {:unknown, 'i'}
[19905]  {:unknown, 'l'}
[19906]  {:unknown, 'e'}
[19907]  {:unknown, ' '}
[19908]  {:unknown, 't'}
[19909]  {:unknown, 'h'}
[19910]  {:unknown, 'e'}
[19911]  {:unknown, ' '}
[19912]  {:unknown, 'l'}
[19913]  {:unknown, 'o'}
[19914]  {:unknown, 'w'}
[19915]  {:unknown, 'e'}
[19916]  {:unknown, 'r'}
[19917]  {:unknown, ' '}
[19918]  {:unknown, 'r'}
[19919]  {:unknown, 'e'}
[19920]  {:unknown, 'g'}
[19921]  {:unknown, 'i'}
[19922]  {:unknown, 's'}
[19923]  {:unknown, 't'}
[19924]  {:unknown, 'e'}
[19925]  {:unknown, 'r'}
[19926]  {:unknown, 's'}
[19927]  {:unknown, ' '}
[19928]  {:unknown, 'p'}
[19929]  {:unknown, 'r'}
[19930]  {:unknown, 'i'}
[19931]  {:unknown, 'm'}
[19932]  {:unknown, 'a'}
[19933]  {:unknown, 'r'}
[19934]  {:unknown, 'i'}
[19935]  {:unknown, 'l'}
[19936]  {:unknown, 'y'}
[19937]  {:unknown, ' '}
[19938]  {:unknown, 'c'}
[19939]  {:unknown, 'o'}
[19940]  {:unknown, 'n'}
[19941]  {:unknown, 't'}
[19942]  {:unknown, 'r'}
[19943]  {:unknown, 'o'}
[19944]  {:unknown, 'l'}
[19945]  {:unknown, ' '}
[19946]  {:unknown, 'm'}
[19947]  {:unknown, 'u'}
[19948]  {:unknown, 'n'}
[19949]  {:unknown, 'd'}
[19950]  {:unknown, 'a'}
[19951]  {:unknown, 'n'}
[19952]  {:unknown, 'e'}
[19953]  {:mult, {:value, 116}, {:value, 104}, {:value, 105}}
[19957]  {:unknown, 'n'}
[19958]  {:unknown, 'g'}
[19959]  {:unknown, 's'}
[19960]  {:unknown, ' '}
[19961]  {:unknown, 'l'}
[19962]  {:unknown, 'i'}
[19963]  {:unknown, 'k'}
[19964]  {:unknown, 'e'}
[19965]  {:unknown, ' '}
[19966]  {:unknown, 's'}
[19967]  {:unknown, 'o'}
[19968]  {:unknown, 'u'}
[19969]  {:unknown, 'n'}
[19970]  {:unknown, 'd'}
[19971]  {:unknown, ' '}
[19972]  {:unknown, 'a'}
[19973]  {:unknown, 'n'}
[19974]  {:unknown, 'd'}
[19975]  {:unknown, ' '}
[19976]  {:unknown, 'l'}
[19977]  {:unknown, 'i'}
[19978]  {:unknown, 'g'}
[19979]  {:unknown, 'h'}
[19980]  {:unknown, 't'}
[19981]  {:unknown, ','}
[19982]  {:unknown, ' '}
[19983]  {:unknown, 't'}
[19984]  {:unknown, 'h'}
[19985]  {:unknown, 'e'}
[19986]  {:unknown, ' '}
[19987]  {:unknown, 'h'}
[19988]  {:unknown, 'i'}
[19989]  {:unknown, 'g'}
[19990]  {:unknown, 'h'}
[19991]  {:unknown, 'e'}
[19992]  {:unknown, 's'}
[19993]  {:unknown, 't'}
[19994]  {:unknown, ' '}
[19995]  {:unknown, 'r'}
[19996]  {:unknown, 'e'}
[19997]  {:unknown, 'g'}
[19998]  {:unknown, 'i'}
[19999]  {:unknown, 's'}
[20000]  {:unknown, 't'}
[20001]  {:unknown, 'e'}
[20002]  {:unknown, 'r'}
[20003]  {:unknown, ' '}
[20004]  {:unknown, '('}
[20005]  {:unknown, 't'}
[20006]  {:unknown, 'h'}
[20007]  {:unknown, 'e'}
[20008]  {:unknown, ' '}
[20009]  {:unknown, 's'}
[20010]  {:unknown, 'o'}
[20011]  {:unknown, '-'}
[20012]  {:unknown, 'c'}
[20013]  {:unknown, 'a'}
[20014]  {:unknown, 'l'}
[20015]  {:unknown, 'l'}
[20016]  {:unknown, 'e'}
[20017]  {:unknown, 'd'}
[20018]  {:unknown, ' '}
[20019]  {:unknown, '"'}
[20020]  {:unknown, 'e'}
[20021]  {:unknown, 'i'}
[20022]  {:unknown, 'g'}
[20023]  {:unknown, 'h'}
[20024]  {:unknown, 't'}
[20025]  {:unknown, 'h'}
[20026]  {:mult, {:value, 114}, {:value, 101}, {:value, 103}}
[20030]  {:unknown, 'i'}
[20031]  {:unknown, 's'}
[20032]  {:unknown, 't'}
[20033]  {:unknown, 'e'}
[20034]  {:unknown, 'r'}
[20035]  {:unknown, '"'}
[20036]  {:unknown, ')'}
[20037]  {:unknown, ' '}
[20038]  {:unknown, 'i'}
[20039]  {:unknown, 's'}
[20040]  {:unknown, ' '}
[20041]  {:unknown, 'u'}
[20042]  {:unknown, 's'}
[20043]  {:unknown, 'e'}
[20044]  {:unknown, 'd'}
[20045]  {:unknown, ' '}
[20046]  {:unknown, 't'}
[20047]  {:unknown, 'o'}
[20048]  {:unknown, ' '}
[20049]  {:unknown, 'c'}
[20050]  {:unknown, 'o'}
[20051]  {:unknown, 'n'}
[20052]  {:unknown, 't'}
[20053]  {:unknown, 'r'}
[20054]  {:unknown, 'o'}
[20055]  {:unknown, 'l'}
[20056]  {:unknown, ' '}
[20057]  {:unknown, 'i'}
[20058]  {:unknown, 'n'}
[20059]  {:unknown, 't'}
[20060]  {:unknown, 'e'}
[20061]  {:unknown, 'r'}
[20062]  {:unknown, 'd'}
[20063]  {:unknown, 'i'}
[20064]  {:unknown, 'm'}
[20065]  {:unknown, 'e'}
[20066]  {:unknown, 'n'}
[20067]  {:unknown, 's'}
[20068]  {:unknown, 'i'}
[20069]  {:unknown, 'o'}
[20070]  {:unknown, 'n'}
[20071]  {:unknown, 'a'}
[20072]  {:unknown, 'l'}
[20073]  {:unknown, ' '}
[20074]  {:unknown, 'e'}
[20075]  {:unknown, 'v'}
[20076]  {:unknown, 'e'}
[20077]  {:unknown, 'n'}
[20078]  {:unknown, 't'}
[20079]  {:unknown, 's'}
[20080]  {:unknown, ' '}
[20081]  {:unknown, 's'}
[20082]  {:unknown, 'u'}
[20083]  {:unknown, 'c'}
[20084]  {:unknown, 'h'}
[20085]  {:unknown, ' '}
[20086]  {:unknown, 'a'}
[20087]  {:unknown, 's'}
[20088]  {:unknown, ' '}
[20089]  {:unknown, 't'}
[20090]  {:unknown, 'e'}
[20091]  {:unknown, 'l'}
[20092]  {:unknown, 'e'}
[20093]  {:unknown, 'p'}
[20094]  {:unknown, 'o'}
[20095]  {:unknown, 'r'}
[20096]  {:unknown, 't'}
[20097]  {:unknown, 'a'}
[20098]  {:unknown, 't'}
[20099]  {:unknown, 'i'}
[20100]  {:unknown, 'o'}
[20101]  {:unknown, 'n'}
[20102]  {:unknown, '.'}
[20103]  {:mult, {:value, 10}, {:value, 65}, {:value, 32}}
[20107]  {:unknown, 'h'}
[20108]  {:unknown, 'y'}
[20109]  {:unknown, 'p'}
[20110]  {:unknown, 'o'}
[20111]  {:unknown, 't'}
[20112]  {:unknown, 'h'}
[20113]  {:unknown, 'e'}
[20114]  {:unknown, 't'}
[20115]  {:unknown, 'i'}
[20116]  {:unknown, 'c'}
[20117]  {:unknown, 'a'}
[20118]  {:unknown, 'l'}
[20119]  {:unknown, ' '}
[20120]  {:unknown, 's'}
[20121]  {:unknown, 'u'}
[20122]  {:unknown, 'c'}
[20123]  {:unknown, 'h'}
[20124]  {:unknown, ' '}
[20125]  {:unknown, 't'}
[20126]  {:unknown, 'e'}
[20127]  {:unknown, 'l'}
[20128]  {:unknown, 'e'}
[20129]  {:unknown, 'p'}
[20130]  {:unknown, 'o'}
[20131]  {:unknown, 'r'}
[20132]  {:unknown, 't'}
[20133]  {:unknown, 'a'}
[20134]  {:unknown, 't'}
[20135]  {:unknown, 'i'}
[20136]  {:unknown, 'o'}
[20137]  {:unknown, 'n'}
[20138]  {:unknown, ' '}
[20139]  {:unknown, 'd'}
[20140]  {:unknown, 'e'}
[20141]  {:unknown, 'v'}
[20142]  {:unknown, 'i'}
[20143]  {:unknown, 'c'}
[20144]  {:unknown, 'e'}
[20145]  {:unknown, ' '}
[20146]  {:unknown, 'w'}
[20147]  {:unknown, 'o'}
[20148]  {:unknown, 'u'}
[20149]  {:unknown, 'l'}
[20150]  {:unknown, 'd'}
[20151]  {:unknown, ' '}
[20152]  {:unknown, 'n'}
[20153]  {:unknown, 'e'}
[20154]  {:unknown, 'e'}
[20155]  {:unknown, 'd'}
[20156]  {:unknown, ' '}
[20157]  {:unknown, 't'}
[20158]  {:unknown, 'o'}
[20159]  {:unknown, ' '}
[20160]  {:unknown, 'h'}
[20161]  {:unknown, 'a'}
[20162]  {:unknown, 'v'}
[20163]  {:unknown, 'e'}
[20164]  {:unknown, ' '}
[20165]  {:unknown, 'h'}
[20166]  {:unknown, 'a'}
[20167]  {:unknown, 'v'}
[20168]  {:unknown, 'e'}
[20169]  {:unknown, ' '}
[20170]  {:unknown, 'e'}
[20171]  {:unknown, 'x'}
[20172]  {:unknown, 'a'}
[20173]  {:unknown, 'c'}
[20174]  {:unknown, 't'}
[20175]  {:unknown, 'l'}
[20176]  {:unknown, 'y'}
[20177]  {:unknown, ' '}
[20178]  {:unknown, 't'}
[20179]  {:unknown, 'w'}
[20180]  {:unknown, 'o'}
[20181]  {:mult, {:value, 100}, {:value, 101}, {:value, 115}}
[20185]  {:unknown, 't'}
[20186]  {:unknown, 'i'}
[20187]  {:unknown, 'n'}
[20188]  {:unknown, 'a'}
[20189]  {:unknown, 't'}
[20190]  {:unknown, 'i'}
[20191]  {:unknown, 'o'}
[20192]  {:unknown, 'n'}
[20193]  {:unknown, 's'}
[20194]  {:unknown, '.'}
[20195]  {:unknown, ' '}
[20196]  {:unknown, ' '}
[20197]  {:unknown, 'O'}
[20198]  {:unknown, 'n'}
[20199]  {:unknown, 'e'}
[20200]  {:unknown, ' '}
[20201]  {:unknown, 'd'}
[20202]  {:unknown, 'e'}
[20203]  {:unknown, 's'}
[20204]  {:unknown, 't'}
[20205]  {:unknown, 'i'}
[20206]  {:unknown, 'n'}
[20207]  {:unknown, 'a'}
[20208]  {:unknown, 't'}
[20209]  {:unknown, 'i'}
[20210]  {:unknown, 'o'}
[20211]  {:unknown, 'n'}
[20212]  {:unknown, ' '}
[20213]  {:unknown, 'w'}
[20214]  {:unknown, 'o'}
[20215]  {:unknown, 'u'}
[20216]  {:unknown, 'l'}
[20217]  {:unknown, 'd'}
[20218]  {:unknown, ' '}
[20219]  {:unknown, 'b'}
[20220]  {:unknown, 'e'}
[20221]  {:unknown, ' '}
[20222]  {:unknown, 'u'}
[20223]  {:unknown, 's'}
[20224]  {:unknown, 'e'}
[20225]  {:unknown, 'd'}
[20226]  {:unknown, ' '}
[20227]  {:unknown, 'w'}
[20228]  {:unknown, 'h'}
[20229]  {:unknown, 'e'}
[20230]  {:unknown, 'n'}
[20231]  {:unknown, ' '}
[20232]  {:unknown, 't'}
[20233]  {:unknown, 'h'}
[20234]  {:unknown, 'e'}
[20235]  {:unknown, ' '}
[20236]  {:unknown, 'e'}
[20237]  {:unknown, 'i'}
[20238]  {:unknown, 'g'}
[20239]  {:unknown, 'h'}
[20240]  {:unknown, 't'}
[20241]  {:unknown, 'h'}
[20242]  {:unknown, ' '}
[20243]  {:unknown, 'r'}
[20244]  {:unknown, 'e'}
[20245]  {:unknown, 'g'}
[20246]  {:unknown, 'i'}
[20247]  {:unknown, 's'}
[20248]  {:unknown, 't'}
[20249]  {:unknown, 'e'}
[20250]  {:unknown, 'r'}
[20251]  {:unknown, ' '}
[20252]  {:unknown, 'i'}
[20253]  {:unknown, 's'}
[20254]  {:unknown, ' '}
[20255]  {:unknown, 'a'}
[20256]  {:unknown, 't'}
[20257]  {:unknown, ' '}
[20258]  {:unknown, 'i'}
[20259]  {:unknown, 't'}
[20260]  {:unknown, 's'}
[20261]  {:mult, {:value, 109}, {:value, 105}, {:value, 110}}
[20265]  {:unknown, 'i'}
[20266]  {:unknown, 'm'}
[20267]  {:unknown, 'u'}
[20268]  {:unknown, 'm'}
[20269]  {:unknown, ' '}
[20270]  {:unknown, 'e'}
[20271]  {:unknown, 'n'}
[20272]  {:unknown, 'e'}
[20273]  {:unknown, 'r'}
[20274]  {:unknown, 'g'}
[20275]  {:unknown, 'y'}
[20276]  {:unknown, ' '}
[20277]  {:unknown, 'l'}
[20278]  {:unknown, 'e'}
[20279]  {:unknown, 'v'}
[20280]  {:unknown, 'e'}
[20281]  {:unknown, 'l'}
[20282]  {:unknown, ' '}
[20283]  {:unknown, '-'}
[20284]  {:unknown, ' '}
[20285]  {:unknown, 't'}
[20286]  {:unknown, 'h'}
[20287]  {:unknown, 'i'}
[20288]  {:unknown, 's'}
[20289]  {:unknown, ' '}
[20290]  {:unknown, 'w'}
[20291]  {:unknown, 'o'}
[20292]  {:unknown, 'u'}
[20293]  {:unknown, 'l'}
[20294]  {:unknown, 'd'}
[20295]  {:unknown, ' '}
[20296]  {:unknown, 'b'}
[20297]  {:unknown, 'e'}
[20298]  {:unknown, ' '}
[20299]  {:unknown, 't'}
[20300]  {:unknown, 'h'}
[20301]  {:unknown, 'e'}
[20302]  {:unknown, ' '}
[20303]  {:unknown, 'd'}
[20304]  {:unknown, 'e'}
[20305]  {:unknown, 'f'}
[20306]  {:unknown, 'a'}
[20307]  {:unknown, 'u'}
[20308]  {:unknown, 'l'}
[20309]  {:unknown, 't'}
[20310]  {:unknown, ' '}
[20311]  {:unknown, 'o'}
[20312]  {:unknown, 'p'}
[20313]  {:unknown, 'e'}
[20314]  {:unknown, 'r'}
[20315]  {:unknown, 'a'}
[20316]  {:unknown, 't'}
[20317]  {:unknown, 'i'}
[20318]  {:unknown, 'o'}
[20319]  {:unknown, 'n'}
[20320]  {:unknown, ' '}
[20321]  {:unknown, 'a'}
[20322]  {:unknown, 's'}
[20323]  {:unknown, 's'}
[20324]  {:unknown, 'u'}
[20325]  {:unknown, 'm'}
[20326]  {:unknown, 'i'}
[20327]  {:unknown, 'n'}
[20328]  {:unknown, 'g'}
[20329]  {:unknown, ' '}
[20330]  {:unknown, 't'}
[20331]  {:unknown, 'h'}
[20332]  {:unknown, 'e'}
[20333]  {:unknown, ' '}
[20334]  {:unknown, 'u'}
[20335]  {:unknown, 's'}
[20336]  {:unknown, 'e'}
[20337]  {:unknown, 'r'}
[20338]  {:mult, {:value, 104}, {:value, 97}, {:value, 115}}
[20342]  {:unknown, ' '}
[20343]  {:unknown, 'n'}
[20344]  {:unknown, 'o'}
[20345]  {:unknown, ' '}
[20346]  {:unknown, 'w'}
[20347]  {:unknown, 'a'}
[20348]  {:unknown, 'y'}
[20349]  {:unknown, ' '}
[20350]  {:unknown, 't'}
[20351]  {:unknown, 'o'}
[20352]  {:unknown, ' '}
[20353]  {:unknown, 'c'}
[20354]  {:unknown, 'o'}
[20355]  {:unknown, 'n'}
[20356]  {:unknown, 't'}
[20357]  {:unknown, 'r'}
[20358]  {:unknown, 'o'}
[20359]  {:unknown, 'l'}
[20360]  {:unknown, ' '}
[20361]  {:unknown, 't'}
[20362]  {:unknown, 'h'}
[20363]  {:unknown, 'e'}
[20364]  {:unknown, ' '}
[20365]  {:unknown, 'e'}
[20366]  {:unknown, 'i'}
[20367]  {:unknown, 'g'}
[20368]  {:unknown, 'h'}
[20369]  {:unknown, 't'}
[20370]  {:unknown, 'h'}
[20371]  {:unknown, ' '}
[20372]  {:unknown, 'r'}
[20373]  {:unknown, 'e'}
[20374]  {:unknown, 'g'}
[20375]  {:unknown, 'i'}
[20376]  {:unknown, 's'}
[20377]  {:unknown, 't'}
[20378]  {:unknown, 'e'}
[20379]  {:unknown, 'r'}
[20380]  {:unknown, '.'}
[20381]  {:unknown, ' '}
[20382]  {:unknown, ' '}
[20383]  {:unknown, 'I'}
[20384]  {:unknown, 'n'}
[20385]  {:unknown, ' '}
[20386]  {:unknown, 't'}
[20387]  {:unknown, 'h'}
[20388]  {:unknown, 'i'}
[20389]  {:unknown, 's'}
[20390]  {:unknown, ' '}
[20391]  {:unknown, 's'}
[20392]  {:unknown, 'i'}
[20393]  {:unknown, 't'}
[20394]  {:unknown, 'u'}
[20395]  {:unknown, 'a'}
[20396]  {:unknown, 't'}
[20397]  {:unknown, 'i'}
[20398]  {:unknown, 'o'}
[20399]  {:unknown, 'n'}
[20400]  {:unknown, ','}
[20401]  {:unknown, ' '}
[20402]  {:unknown, 't'}
[20403]  {:unknown, 'h'}
[20404]  {:unknown, 'e'}
[20405]  {:unknown, ' '}
[20406]  {:unknown, 't'}
[20407]  {:unknown, 'e'}
[20408]  {:unknown, 'l'}
[20409]  {:unknown, 'e'}
[20410]  {:unknown, 'p'}
[20411]  {:unknown, 'o'}
[20412]  {:unknown, 'r'}
[20413]  {:unknown, 't'}
[20414]  {:unknown, 'e'}
[20415]  {:unknown, 'r'}
[20416]  {:mult, {:value, 115}, {:value, 104}, {:value, 111}}
[20420]  {:unknown, 'u'}
[20421]  {:unknown, 'l'}
[20422]  {:unknown, 'd'}
[20423]  {:unknown, ' '}
[20424]  {:unknown, 's'}
[20425]  {:unknown, 'e'}
[20426]  {:unknown, 'n'}
[20427]  {:unknown, 'd'}
[20428]  {:unknown, ' '}
[20429]  {:unknown, 't'}
[20430]  {:unknown, 'h'}
[20431]  {:unknown, 'e'}
[20432]  {:unknown, ' '}
[20433]  {:unknown, 'u'}
[20434]  {:unknown, 's'}
[20435]  {:unknown, 'e'}
[20436]  {:unknown, 'r'}
[20437]  {:unknown, ' '}
[20438]  {:unknown, 't'}
[20439]  {:unknown, 'o'}
[20440]  {:unknown, ' '}
[20441]  {:unknown, 'a'}
[20442]  {:unknown, ' '}
[20443]  {:unknown, 'p'}
[20444]  {:unknown, 'r'}
[20445]  {:unknown, 'e'}
[20446]  {:unknown, 'c'}
[20447]  {:unknown, 'o'}
[20448]  {:unknown, 'n'}
[20449]  {:unknown, 'f'}
[20450]  {:unknown, 'i'}
[20451]  {:unknown, 'g'}
[20452]  {:unknown, 'u'}
[20453]  {:unknown, 'r'}
[20454]  {:unknown, 'e'}
[20455]  {:unknown, 'd'}
[20456]  {:unknown, ' '}
[20457]  {:unknown, 's'}
[20458]  {:unknown, 'a'}
[20459]  {:unknown, 'f'}
[20460]  {:unknown, 'e'}
[20461]  {:unknown, ' '}
[20462]  {:unknown, 'l'}
[20463]  {:unknown, 'o'}
[20464]  {:unknown, 'c'}
[20465]  {:unknown, 'a'}
[20466]  {:unknown, 't'}
[20467]  {:unknown, 'i'}
[20468]  {:unknown, 'o'}
[20469]  {:unknown, 'n'}
[20470]  {:unknown, ' '}
[20471]  {:unknown, 'a'}
[20472]  {:unknown, 's'}
[20473]  {:unknown, ' '}
[20474]  {:unknown, 'a'}
[20475]  {:unknown, ' '}
[20476]  {:unknown, 'd'}
[20477]  {:unknown, 'e'}
[20478]  {:unknown, 'f'}
[20479]  {:unknown, 'a'}
[20480]  {:unknown, 'u'}
[20481]  {:unknown, 'l'}
[20482]  {:unknown, 't'}
[20483]  {:unknown, '.'}
[20484]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[20488]  {:unknown, 'e'}
[20489]  {:unknown, ' '}
[20490]  {:unknown, 's'}
[20491]  {:unknown, 'e'}
[20492]  {:unknown, 'c'}
[20493]  {:unknown, 'o'}
[20494]  {:unknown, 'n'}
[20495]  {:unknown, 'd'}
[20496]  {:unknown, ' '}
[20497]  {:unknown, 'd'}
[20498]  {:unknown, 'e'}
[20499]  {:unknown, 's'}
[20500]  {:unknown, 't'}
[20501]  {:unknown, 'i'}
[20502]  {:unknown, 'n'}
[20503]  {:unknown, 'a'}
[20504]  {:unknown, 't'}
[20505]  {:unknown, 'i'}
[20506]  {:unknown, 'o'}
[20507]  {:unknown, 'n'}
[20508]  {:unknown, ','}
[20509]  {:unknown, ' '}
[20510]  {:unknown, 'h'}
[20511]  {:unknown, 'o'}
[20512]  {:unknown, 'w'}
[20513]  {:unknown, 'e'}
[20514]  {:unknown, 'v'}
[20515]  {:unknown, 'e'}
[20516]  {:unknown, 'r'}
[20517]  {:unknown, ','}
[20518]  {:unknown, ' '}
[20519]  {:unknown, 'i'}
[20520]  {:unknown, 's'}
[20521]  {:unknown, ' '}
[20522]  {:unknown, 'p'}
[20523]  {:unknown, 'r'}
[20524]  {:unknown, 'e'}
[20525]  {:unknown, 'd'}
[20526]  {:unknown, 'i'}
[20527]  {:unknown, 'c'}
[20528]  {:unknown, 't'}
[20529]  {:unknown, 'e'}
[20530]  {:unknown, 'd'}
[20531]  {:unknown, ' '}
[20532]  {:unknown, 't'}
[20533]  {:unknown, 'o'}
[20534]  {:unknown, ' '}
[20535]  {:unknown, 'r'}
[20536]  {:unknown, 'e'}
[20537]  {:unknown, 'q'}
[20538]  {:unknown, 'u'}
[20539]  {:unknown, 'i'}
[20540]  {:unknown, 'r'}
[20541]  {:unknown, 'e'}
[20542]  {:unknown, ' '}
[20543]  {:unknown, 'a'}
[20544]  {:unknown, ' '}
[20545]  {:unknown, 'v'}
[20546]  {:unknown, 'e'}
[20547]  {:unknown, 'r'}
[20548]  {:unknown, 'y'}
[20549]  {:unknown, ' '}
[20550]  {:unknown, 's'}
[20551]  {:unknown, 'p'}
[20552]  {:unknown, 'e'}
[20553]  {:unknown, 'c'}
[20554]  {:unknown, 'i'}
[20555]  {:unknown, 'f'}
[20556]  {:unknown, 'i'}
[20557]  {:unknown, 'c'}
[20558]  {:mult, {:value, 101}, {:value, 110}, {:value, 101}}
[20562]  {:unknown, 'r'}
[20563]  {:unknown, 'g'}
[20564]  {:unknown, 'y'}
[20565]  {:unknown, ' '}
[20566]  {:unknown, 'l'}
[20567]  {:unknown, 'e'}
[20568]  {:unknown, 'v'}
[20569]  {:unknown, 'e'}
[20570]  {:unknown, 'l'}
[20571]  {:unknown, ' '}
[20572]  {:unknown, 'i'}
[20573]  {:unknown, 'n'}
[20574]  {:unknown, ' '}
[20575]  {:unknown, 't'}
[20576]  {:unknown, 'h'}
[20577]  {:unknown, 'e'}
[20578]  {:unknown, ' '}
[20579]  {:unknown, 'e'}
[20580]  {:unknown, 'i'}
[20581]  {:unknown, 'g'}
[20582]  {:unknown, 'h'}
[20583]  {:unknown, 't'}
[20584]  {:unknown, 'h'}
[20585]  {:unknown, ' '}
[20586]  {:unknown, 'r'}
[20587]  {:unknown, 'e'}
[20588]  {:unknown, 'g'}
[20589]  {:unknown, 'i'}
[20590]  {:unknown, 's'}
[20591]  {:unknown, 't'}
[20592]  {:unknown, 'e'}
[20593]  {:unknown, 'r'}
[20594]  {:unknown, '.'}
[20595]  {:unknown, ' '}
[20596]  {:unknown, ' '}
[20597]  {:unknown, 'T'}
[20598]  {:unknown, 'h'}
[20599]  {:unknown, 'e'}
[20600]  {:unknown, ' '}
[20601]  {:unknown, 't'}
[20602]  {:unknown, 'e'}
[20603]  {:unknown, 'l'}
[20604]  {:unknown, 'e'}
[20605]  {:unknown, 'p'}
[20606]  {:unknown, 'o'}
[20607]  {:unknown, 'r'}
[20608]  {:unknown, 't'}
[20609]  {:unknown, 'e'}
[20610]  {:unknown, 'r'}
[20611]  {:unknown, ' '}
[20612]  {:unknown, 'm'}
[20613]  {:unknown, 'u'}
[20614]  {:unknown, 's'}
[20615]  {:unknown, 't'}
[20616]  {:unknown, ' '}
[20617]  {:unknown, 't'}
[20618]  {:unknown, 'a'}
[20619]  {:unknown, 'k'}
[20620]  {:unknown, 'e'}
[20621]  {:unknown, ' '}
[20622]  {:unknown, 'g'}
[20623]  {:unknown, 'r'}
[20624]  {:unknown, 'e'}
[20625]  {:unknown, 'a'}
[20626]  {:unknown, 't'}
[20627]  {:unknown, ' '}
[20628]  {:unknown, 'c'}
[20629]  {:unknown, 'a'}
[20630]  {:unknown, 'r'}
[20631]  {:unknown, 'e'}
[20632]  {:unknown, ' '}
[20633]  {:unknown, 't'}
[20634]  {:unknown, 'o'}
[20635]  {:mult, {:value, 99}, {:value, 111}, {:value, 110}}
[20639]  {:unknown, 'f'}
[20640]  {:unknown, 'i'}
[20641]  {:unknown, 'r'}
[20642]  {:unknown, 'm'}
[20643]  {:unknown, ' '}
[20644]  {:unknown, 't'}
[20645]  {:unknown, 'h'}
[20646]  {:unknown, 'a'}
[20647]  {:unknown, 't'}
[20648]  {:unknown, ' '}
[20649]  {:unknown, 't'}
[20650]  {:unknown, 'h'}
[20651]  {:unknown, 'i'}
[20652]  {:unknown, 's'}
[20653]  {:unknown, ' '}
[20654]  {:unknown, 'e'}
[20655]  {:unknown, 'n'}
[20656]  {:unknown, 'e'}
[20657]  {:unknown, 'r'}
[20658]  {:unknown, 'g'}
[20659]  {:unknown, 'y'}
[20660]  {:unknown, ' '}
[20661]  {:unknown, 'l'}
[20662]  {:unknown, 'e'}
[20663]  {:unknown, 'v'}
[20664]  {:unknown, 'e'}
[20665]  {:unknown, 'l'}
[20666]  {:unknown, ' '}
[20667]  {:unknown, 'i'}
[20668]  {:unknown, 's'}
[20669]  {:unknown, ' '}
[20670]  {:unknown, 'e'}
[20671]  {:unknown, 'x'}
[20672]  {:unknown, 'a'}
[20673]  {:unknown, 'c'}
[20674]  {:unknown, 't'}
[20675]  {:unknown, 'l'}
[20676]  {:unknown, 'y'}
[20677]  {:unknown, ' '}
[20678]  {:unknown, 'c'}
[20679]  {:unknown, 'o'}
[20680]  {:unknown, 'r'}
[20681]  {:unknown, 'r'}
[20682]  {:unknown, 'e'}
[20683]  {:unknown, 'c'}
[20684]  {:unknown, 't'}
[20685]  {:unknown, ' '}
[20686]  {:unknown, 'b'}
[20687]  {:unknown, 'e'}
[20688]  {:unknown, 'f'}
[20689]  {:unknown, 'o'}
[20690]  {:unknown, 'r'}
[20691]  {:unknown, 'e'}
[20692]  {:unknown, ' '}
[20693]  {:unknown, 't'}
[20694]  {:unknown, 'e'}
[20695]  {:unknown, 'l'}
[20696]  {:unknown, 'e'}
[20697]  {:unknown, 'p'}
[20698]  {:unknown, 'o'}
[20699]  {:unknown, 'r'}
[20700]  {:unknown, 't'}
[20701]  {:unknown, 'i'}
[20702]  {:unknown, 'n'}
[20703]  {:unknown, 'g'}
[20704]  {:unknown, ' '}
[20705]  {:unknown, 'i'}
[20706]  {:unknown, 't'}
[20707]  {:unknown, 's'}
[20708]  {:unknown, ' '}
[20709]  {:unknown, 'u'}
[20710]  {:unknown, 's'}
[20711]  {:unknown, 'e'}
[20712]  {:unknown, 'r'}
[20713]  {:unknown, '!'}
[20714]  {:mult, {:value, 73}, {:value, 102}, {:value, 32}}
[20718]  {:unknown, 'i'}
[20719]  {:unknown, 't'}
[20720]  {:unknown, ' '}
[20721]  {:unknown, 'i'}
[20722]  {:unknown, 's'}
[20723]  {:unknown, ' '}
[20724]  {:unknown, 'e'}
[20725]  {:unknown, 'v'}
[20726]  {:unknown, 'e'}
[20727]  {:unknown, 'n'}
[20728]  {:unknown, ' '}
[20729]  {:unknown, 's'}
[20730]  {:unknown, 'l'}
[20731]  {:unknown, 'i'}
[20732]  {:unknown, 'g'}
[20733]  {:unknown, 'h'}
[20734]  {:unknown, 't'}
[20735]  {:unknown, 'l'}
[20736]  {:unknown, 'y'}
[20737]  {:unknown, ' '}
[20738]  {:unknown, 'o'}
[20739]  {:unknown, 'f'}
[20740]  {:unknown, 'f'}
[20741]  {:unknown, ','}
[20742]  {:unknown, ' '}
[20743]  {:unknown, 't'}
[20744]  {:unknown, 'h'}
[20745]  {:unknown, 'e'}
[20746]  {:unknown, ' '}
[20747]  {:unknown, 'u'}
[20748]  {:unknown, 's'}
[20749]  {:unknown, 'e'}
[20750]  {:unknown, 'r'}
[20751]  {:unknown, ' '}
[20752]  {:unknown, 'w'}
[20753]  {:unknown, 'o'}
[20754]  {:unknown, 'u'}
[20755]  {:unknown, 'l'}
[20756]  {:unknown, 'd'}
[20757]  {:unknown, ' '}
[20758]  {:unknown, '('}
[20759]  {:unknown, 'p'}
[20760]  {:unknown, 'r'}
[20761]  {:unknown, 'o'}
[20762]  {:unknown, 'b'}
[20763]  {:unknown, 'a'}
[20764]  {:unknown, 'b'}
[20765]  {:unknown, 'l'}
[20766]  {:unknown, 'y'}
[20767]  {:unknown, ')'}
[20768]  {:unknown, ' '}
[20769]  {:unknown, 'a'}
[20770]  {:unknown, 'r'}
[20771]  {:unknown, 'r'}
[20772]  {:unknown, 'i'}
[20773]  {:unknown, 'v'}
[20774]  {:unknown, 'e'}
[20775]  {:unknown, ' '}
[20776]  {:unknown, 'a'}
[20777]  {:unknown, 't'}
[20778]  {:unknown, ' '}
[20779]  {:unknown, 't'}
[20780]  {:unknown, 'h'}
[20781]  {:unknown, 'e'}
[20782]  {:unknown, ' '}
[20783]  {:unknown, 'c'}
[20784]  {:unknown, 'o'}
[20785]  {:unknown, 'r'}
[20786]  {:unknown, 'r'}
[20787]  {:unknown, 'e'}
[20788]  {:unknown, 'c'}
[20789]  {:unknown, 't'}
[20790]  {:mult, {:value, 108}, {:value, 111}, {:value, 99}}
[20794]  {:unknown, 'a'}
[20795]  {:unknown, 't'}
[20796]  {:unknown, 'i'}
[20797]  {:unknown, 'o'}
[20798]  {:unknown, 'n'}
[20799]  {:unknown, ','}
[20800]  {:unknown, ' '}
[20801]  {:unknown, 'b'}
[20802]  {:unknown, 'u'}
[20803]  {:unknown, 't'}
[20804]  {:unknown, ' '}
[20805]  {:unknown, 'w'}
[20806]  {:unknown, 'o'}
[20807]  {:unknown, 'u'}
[20808]  {:unknown, 'l'}
[20809]  {:unknown, 'd'}
[20810]  {:unknown, ' '}
[20811]  {:unknown, 'b'}
[20812]  {:unknown, 'r'}
[20813]  {:unknown, 'i'}
[20814]  {:unknown, 'e'}
[20815]  {:unknown, 'f'}
[20816]  {:unknown, 'l'}
[20817]  {:unknown, 'y'}
[20818]  {:unknown, ' '}
[20819]  {:unknown, 'e'}
[20820]  {:unknown, 'x'}
[20821]  {:unknown, 'p'}
[20822]  {:unknown, 'e'}
[20823]  {:unknown, 'r'}
[20824]  {:unknown, 'i'}
[20825]  {:unknown, 'e'}
[20826]  {:unknown, 'n'}
[20827]  {:unknown, 'c'}
[20828]  {:unknown, 'e'}
[20829]  {:unknown, ' '}
[20830]  {:unknown, 'a'}
[20831]  {:unknown, 'n'}
[20832]  {:unknown, 'o'}
[20833]  {:unknown, 'm'}
[20834]  {:unknown, 'a'}
[20835]  {:unknown, 'l'}
[20836]  {:unknown, 'i'}
[20837]  {:unknown, 'e'}
[20838]  {:unknown, 's'}
[20839]  {:unknown, ' '}
[20840]  {:unknown, 'i'}
[20841]  {:unknown, 'n'}
[20842]  {:unknown, ' '}
[20843]  {:unknown, 't'}
[20844]  {:unknown, 'h'}
[20845]  {:unknown, 'e'}
[20846]  {:unknown, ' '}
[20847]  {:unknown, 'f'}
[20848]  {:unknown, 'a'}
[20849]  {:unknown, 'b'}
[20850]  {:unknown, 'r'}
[20851]  {:unknown, 'i'}
[20852]  {:unknown, 'c'}
[20853]  {:unknown, ' '}
[20854]  {:unknown, 'o'}
[20855]  {:unknown, 'f'}
[20856]  {:unknown, ' '}
[20857]  {:unknown, 'r'}
[20858]  {:unknown, 'e'}
[20859]  {:unknown, 'a'}
[20860]  {:unknown, 'l'}
[20861]  {:unknown, 'i'}
[20862]  {:unknown, 't'}
[20863]  {:unknown, 'y'}
[20864]  {:mult, {:value, 105}, {:value, 116}, {:value, 115}}
[20868]  {:unknown, 'e'}
[20869]  {:unknown, 'l'}
[20870]  {:unknown, 'f'}
[20871]  {:unknown, ' '}
[20872]  {:unknown, '-'}
[20873]  {:unknown, ' '}
[20874]  {:unknown, 't'}
[20875]  {:unknown, 'h'}
[20876]  {:unknown, 'i'}
[20877]  {:unknown, 's'}
[20878]  {:unknown, ' '}
[20879]  {:unknown, 'i'}
[20880]  {:unknown, 's'}
[20881]  {:unknown, ','}
[20882]  {:unknown, ' '}
[20883]  {:unknown, 'o'}
[20884]  {:unknown, 'f'}
[20885]  {:unknown, ' '}
[20886]  {:unknown, 'c'}
[20887]  {:unknown, 'o'}
[20888]  {:unknown, 'u'}
[20889]  {:unknown, 'r'}
[20890]  {:unknown, 's'}
[20891]  {:unknown, 'e'}
[20892]  {:unknown, ','}
[20893]  {:unknown, ' '}
[20894]  {:unknown, 'n'}
[20895]  {:unknown, 'o'}
[20896]  {:unknown, 't'}
[20897]  {:unknown, ' '}
[20898]  {:unknown, 'r'}
[20899]  {:unknown, 'e'}
[20900]  {:unknown, 'c'}
[20901]  {:unknown, 'o'}
[20902]  {:unknown, 'm'}
[20903]  {:unknown, 'm'}
[20904]  {:unknown, 'e'}
[20905]  {:unknown, 'n'}
[20906]  {:unknown, 'd'}
[20907]  {:unknown, 'e'}
[20908]  {:unknown, 'd'}
[20909]  {:unknown, '.'}
[20910]  {:unknown, ' '}
[20911]  {:unknown, ' '}
[20912]  {:unknown, 'A'}
[20913]  {:unknown, 'n'}
[20914]  {:unknown, 'y'}
[20915]  {:unknown, ' '}
[20916]  {:unknown, 't'}
[20917]  {:unknown, 'e'}
[20918]  {:unknown, 'l'}
[20919]  {:unknown, 'e'}
[20920]  {:unknown, 'p'}
[20921]  {:unknown, 'o'}
[20922]  {:unknown, 'r'}
[20923]  {:unknown, 't'}
[20924]  {:unknown, 'e'}
[20925]  {:unknown, 'r'}
[20926]  {:unknown, ' '}
[20927]  {:unknown, 'w'}
[20928]  {:unknown, 'o'}
[20929]  {:unknown, 'u'}
[20930]  {:unknown, 'l'}
[20931]  {:unknown, 'd'}
[20932]  {:unknown, ' '}
[20933]  {:unknown, 'n'}
[20934]  {:unknown, 'e'}
[20935]  {:unknown, 'e'}
[20936]  {:unknown, 'd'}
[20937]  {:unknown, ' '}
[20938]  {:unknown, 't'}
[20939]  {:unknown, 'o'}
[20940]  {:unknown, ' '}
[20941]  {:unknown, 't'}
[20942]  {:unknown, 'e'}
[20943]  {:unknown, 's'}
[20944]  {:unknown, 't'}
[20945]  {:mult, {:value, 116}, {:value, 104}, {:value, 101}}
[20949]  {:unknown, ' '}
[20950]  {:unknown, 'e'}
[20951]  {:unknown, 'n'}
[20952]  {:unknown, 'e'}
[20953]  {:unknown, 'r'}
[20954]  {:unknown, 'g'}
[20955]  {:unknown, 'y'}
[20956]  {:unknown, ' '}
[20957]  {:unknown, 'l'}
[20958]  {:unknown, 'e'}
[20959]  {:unknown, 'v'}
[20960]  {:unknown, 'e'}
[20961]  {:unknown, 'l'}
[20962]  {:unknown, ' '}
[20963]  {:unknown, 'i'}
[20964]  {:unknown, 'n'}
[20965]  {:unknown, ' '}
[20966]  {:unknown, 't'}
[20967]  {:unknown, 'h'}
[20968]  {:unknown, 'e'}
[20969]  {:unknown, ' '}
[20970]  {:unknown, 'e'}
[20971]  {:unknown, 'i'}
[20972]  {:unknown, 'g'}
[20973]  {:unknown, 'h'}
[20974]  {:unknown, 't'}
[20975]  {:unknown, 'h'}
[20976]  {:unknown, ' '}
[20977]  {:unknown, 'r'}
[20978]  {:unknown, 'e'}
[20979]  {:unknown, 'g'}
[20980]  {:unknown, 'i'}
[20981]  {:unknown, 's'}
[20982]  {:unknown, 't'}
[20983]  {:unknown, 'e'}
[20984]  {:unknown, 'r'}
[20985]  {:unknown, ' '}
[20986]  {:unknown, 'a'}
[20987]  {:unknown, 'n'}
[20988]  {:unknown, 'd'}
[20989]  {:unknown, ' '}
[20990]  {:unknown, 'a'}
[20991]  {:unknown, 'b'}
[20992]  {:unknown, 'o'}
[20993]  {:unknown, 'r'}
[20994]  {:unknown, 't'}
[20995]  {:unknown, ' '}
[20996]  {:unknown, 't'}
[20997]  {:unknown, 'e'}
[20998]  {:unknown, 'l'}
[20999]  {:unknown, 'e'}
[21000]  {:unknown, 'p'}
[21001]  {:unknown, 'o'}
[21002]  {:unknown, 'r'}
[21003]  {:unknown, 't'}
[21004]  {:unknown, 'a'}
[21005]  {:unknown, 't'}
[21006]  {:unknown, 'i'}
[21007]  {:unknown, 'o'}
[21008]  {:unknown, 'n'}
[21009]  {:unknown, ' '}
[21010]  {:unknown, 'i'}
[21011]  {:unknown, 'f'}
[21012]  {:unknown, ' '}
[21013]  {:unknown, 'i'}
[21014]  {:unknown, 't'}
[21015]  {:unknown, ' '}
[21016]  {:unknown, 'i'}
[21017]  {:unknown, 's'}
[21018]  {:unknown, ' '}
[21019]  {:unknown, 'n'}
[21020]  {:unknown, 'o'}
[21021]  {:unknown, 't'}
[21022]  {:mult, {:value, 101}, {:value, 120}, {:value, 97}}
[21026]  {:unknown, 'c'}
[21027]  {:unknown, 't'}
[21028]  {:unknown, 'l'}
[21029]  {:unknown, 'y'}
[21030]  {:unknown, ' '}
[21031]  {:unknown, 'c'}
[21032]  {:unknown, 'o'}
[21033]  {:unknown, 'r'}
[21034]  {:unknown, 'r'}
[21035]  {:unknown, 'e'}
[21036]  {:unknown, 'c'}
[21037]  {:unknown, 't'}
[21038]  {:unknown, '.'}
[21039]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[21043]  {:unknown, 'i'}
[21044]  {:unknown, 's'}
[21045]  {:unknown, ' '}
[21046]  {:unknown, 'r'}
[21047]  {:unknown, 'e'}
[21048]  {:unknown, 'q'}
[21049]  {:unknown, 'u'}
[21050]  {:unknown, 'i'}
[21051]  {:unknown, 'r'}
[21052]  {:unknown, 'e'}
[21053]  {:unknown, 'd'}
[21054]  {:unknown, ' '}
[21055]  {:unknown, 'p'}
[21056]  {:unknown, 'r'}
[21057]  {:unknown, 'e'}
[21058]  {:unknown, 'c'}
[21059]  {:unknown, 'i'}
[21060]  {:unknown, 's'}
[21061]  {:unknown, 'i'}
[21062]  {:unknown, 'o'}
[21063]  {:unknown, 'n'}
[21064]  {:unknown, ' '}
[21065]  {:unknown, 'i'}
[21066]  {:unknown, 'm'}
[21067]  {:unknown, 'p'}
[21068]  {:unknown, 'l'}
[21069]  {:unknown, 'i'}
[21070]  {:unknown, 'e'}
[21071]  {:unknown, 's'}
[21072]  {:unknown, ' '}
[21073]  {:unknown, 't'}
[21074]  {:unknown, 'h'}
[21075]  {:unknown, 'a'}
[21076]  {:unknown, 't'}
[21077]  {:unknown, ' '}
[21078]  {:unknown, 't'}
[21079]  {:unknown, 'h'}
[21080]  {:unknown, 'e'}
[21081]  {:unknown, ' '}
[21082]  {:unknown, 'c'}
[21083]  {:unknown, 'o'}
[21084]  {:unknown, 'n'}
[21085]  {:unknown, 'f'}
[21086]  {:unknown, 'i'}
[21087]  {:unknown, 'r'}
[21088]  {:unknown, 'm'}
[21089]  {:unknown, 'a'}
[21090]  {:unknown, 't'}
[21091]  {:unknown, 'i'}
[21092]  {:unknown, 'o'}
[21093]  {:unknown, 'n'}
[21094]  {:unknown, ' '}
[21095]  {:unknown, 'm'}
[21096]  {:unknown, 'e'}
[21097]  {:unknown, 'c'}
[21098]  {:unknown, 'h'}
[21099]  {:unknown, 'a'}
[21100]  {:unknown, 'n'}
[21101]  {:unknown, 'i'}
[21102]  {:unknown, 's'}
[21103]  {:unknown, 'm'}
[21104]  {:unknown, ' '}
[21105]  {:unknown, 'w'}
[21106]  {:unknown, 'o'}
[21107]  {:unknown, 'u'}
[21108]  {:unknown, 'l'}
[21109]  {:unknown, 'd'}
[21110]  {:unknown, ' '}
[21111]  {:unknown, 'b'}
[21112]  {:unknown, 'e'}
[21113]  {:unknown, ' '}
[21114]  {:unknown, 'v'}
[21115]  {:unknown, 'e'}
[21116]  {:unknown, 'r'}
[21117]  {:unknown, 'y'}
[21118]  {:mult, {:value, 99}, {:value, 111}, {:value, 109}}
[21122]  {:unknown, 'p'}
[21123]  {:unknown, 'u'}
[21124]  {:unknown, 't'}
[21125]  {:unknown, 'a'}
[21126]  {:unknown, 't'}
[21127]  {:unknown, 'i'}
[21128]  {:unknown, 'o'}
[21129]  {:unknown, 'n'}
[21130]  {:unknown, 'a'}
[21131]  {:unknown, 'l'}
[21132]  {:unknown, 'l'}
[21133]  {:unknown, 'y'}
[21134]  {:unknown, ' '}
[21135]  {:unknown, 'e'}
[21136]  {:unknown, 'x'}
[21137]  {:unknown, 'p'}
[21138]  {:unknown, 'e'}
[21139]  {:unknown, 'n'}
[21140]  {:unknown, 's'}
[21141]  {:unknown, 'i'}
[21142]  {:unknown, 'v'}
[21143]  {:unknown, 'e'}
[21144]  {:unknown, '.'}
[21145]  {:unknown, ' '}
[21146]  {:unknown, ' '}
[21147]  {:unknown, 'W'}
[21148]  {:unknown, 'h'}
[21149]  {:unknown, 'i'}
[21150]  {:unknown, 'l'}
[21151]  {:unknown, 'e'}
[21152]  {:unknown, ' '}
[21153]  {:unknown, 't'}
[21154]  {:unknown, 'h'}
[21155]  {:unknown, 'i'}
[21156]  {:unknown, 's'}
[21157]  {:unknown, ' '}
[21158]  {:unknown, 'w'}
[21159]  {:unknown, 'o'}
[21160]  {:unknown, 'u'}
[21161]  {:unknown, 'l'}
[21162]  {:unknown, 'd'}
[21163]  {:unknown, ' '}
[21164]  {:unknown, 'l'}
[21165]  {:unknown, 'i'}
[21166]  {:unknown, 'k'}
[21167]  {:unknown, 'e'}
[21168]  {:unknown, 'l'}
[21169]  {:unknown, 'y'}
[21170]  {:unknown, ' '}
[21171]  {:unknown, 'n'}
[21172]  {:unknown, 'o'}
[21173]  {:unknown, 't'}
[21174]  {:unknown, ' '}
[21175]  {:unknown, 'b'}
[21176]  {:unknown, 'e'}
[21177]  {:unknown, ' '}
[21178]  {:unknown, 'a'}
[21179]  {:unknown, 'n'}
[21180]  {:unknown, ' '}
[21181]  {:unknown, 'i'}
[21182]  {:unknown, 's'}
[21183]  {:unknown, 's'}
[21184]  {:unknown, 'u'}
[21185]  {:unknown, 'e'}
[21186]  {:unknown, ' '}
[21187]  {:unknown, 'f'}
[21188]  {:unknown, 'o'}
[21189]  {:unknown, 'r'}
[21190]  {:unknown, ' '}
[21191]  {:unknown, 'l'}
[21192]  {:unknown, 'a'}
[21193]  {:unknown, 'r'}
[21194]  {:unknown, 'g'}
[21195]  {:unknown, 'e'}
[21196]  {:unknown, '-'}
[21197]  {:mult, {:value, 115}, {:value, 99}, {:value, 97}}
[21201]  {:unknown, 'l'}
[21202]  {:unknown, 'e'}
[21203]  {:unknown, ' '}
[21204]  {:unknown, 't'}
[21205]  {:unknown, 'e'}
[21206]  {:unknown, 'l'}
[21207]  {:unknown, 'e'}
[21208]  {:unknown, 'p'}
[21209]  {:unknown, 'o'}
[21210]  {:unknown, 'r'}
[21211]  {:unknown, 't'}
[21212]  {:unknown, 'e'}
[21213]  {:unknown, 'r'}
[21214]  {:unknown, 's'}
[21215]  {:unknown, ','}
[21216]  {:unknown, ' '}
[21217]  {:unknown, 'a'}
[21218]  {:unknown, ' '}
[21219]  {:unknown, 'h'}
[21220]  {:unknown, 'y'}
[21221]  {:unknown, 'p'}
[21222]  {:unknown, 'o'}
[21223]  {:unknown, 't'}
[21224]  {:unknown, 'h'}
[21225]  {:unknown, 'e'}
[21226]  {:unknown, 't'}
[21227]  {:unknown, 'i'}
[21228]  {:unknown, 'c'}
[21229]  {:unknown, 'a'}
[21230]  {:unknown, 'l'}
[21231]  {:unknown, ' '}
[21232]  {:unknown, 'h'}
[21233]  {:unknown, 'a'}
[21234]  {:unknown, 'n'}
[21235]  {:unknown, 'd'}
[21236]  {:unknown, '-'}
[21237]  {:unknown, 'h'}
[21238]  {:unknown, 'e'}
[21239]  {:unknown, 'l'}
[21240]  {:unknown, 'd'}
[21241]  {:unknown, ' '}
[21242]  {:unknown, 't'}
[21243]  {:unknown, 'e'}
[21244]  {:unknown, 'l'}
[21245]  {:unknown, 'e'}
[21246]  {:unknown, 'p'}
[21247]  {:unknown, 'o'}
[21248]  {:unknown, 'r'}
[21249]  {:unknown, 't'}
[21250]  {:unknown, 'e'}
[21251]  {:unknown, 'r'}
[21252]  {:unknown, ' '}
[21253]  {:unknown, 'w'}
[21254]  {:unknown, 'o'}
[21255]  {:unknown, 'u'}
[21256]  {:unknown, 'l'}
[21257]  {:unknown, 'd'}
[21258]  {:unknown, ' '}
[21259]  {:unknown, 't'}
[21260]  {:unknown, 'a'}
[21261]  {:unknown, 'k'}
[21262]  {:unknown, 'e'}
[21263]  {:unknown, ' '}
[21264]  {:unknown, 'b'}
[21265]  {:unknown, 'i'}
[21266]  {:unknown, 'l'}
[21267]  {:unknown, 'l'}
[21268]  {:unknown, 'i'}
[21269]  {:unknown, 'o'}
[21270]  {:unknown, 'n'}
[21271]  {:unknown, 's'}
[21272]  {:unknown, ' '}
[21273]  {:unknown, 'o'}
[21274]  {:unknown, 'f'}
[21275]  {:mult, {:value, 121}, {:value, 101}, {:value, 97}}
[21279]  {:unknown, 'r'}
[21280]  {:unknown, 's'}
[21281]  {:unknown, ' '}
[21282]  {:unknown, 't'}
[21283]  {:unknown, 'o'}
[21284]  {:unknown, ' '}
[21285]  {:unknown, 'c'}
[21286]  {:unknown, 'o'}
[21287]  {:unknown, 'm'}
[21288]  {:unknown, 'p'}
[21289]  {:unknown, 'u'}
[21290]  {:unknown, 't'}
[21291]  {:unknown, 'e'}
[21292]  {:unknown, ' '}
[21293]  {:unknown, 't'}
[21294]  {:unknown, 'h'}
[21295]  {:unknown, 'e'}
[21296]  {:unknown, ' '}
[21297]  {:unknown, 'r'}
[21298]  {:unknown, 'e'}
[21299]  {:unknown, 's'}
[21300]  {:unknown, 'u'}
[21301]  {:unknown, 'l'}
[21302]  {:unknown, 't'}
[21303]  {:unknown, ' '}
[21304]  {:unknown, 'a'}
[21305]  {:unknown, 'n'}
[21306]  {:unknown, 'd'}
[21307]  {:unknown, ' '}
[21308]  {:unknown, 'c'}
[21309]  {:unknown, 'o'}
[21310]  {:unknown, 'n'}
[21311]  {:unknown, 'f'}
[21312]  {:unknown, 'i'}
[21313]  {:unknown, 'r'}
[21314]  {:unknown, 'm'}
[21315]  {:unknown, ' '}
[21316]  {:unknown, 't'}
[21317]  {:unknown, 'h'}
[21318]  {:unknown, 'a'}
[21319]  {:unknown, 't'}
[21320]  {:unknown, ' '}
[21321]  {:unknown, 't'}
[21322]  {:unknown, 'h'}
[21323]  {:unknown, 'e'}
[21324]  {:unknown, ' '}
[21325]  {:unknown, 'e'}
[21326]  {:unknown, 'i'}
[21327]  {:unknown, 'g'}
[21328]  {:unknown, 'h'}
[21329]  {:unknown, 't'}
[21330]  {:unknown, 'h'}
[21331]  {:unknown, ' '}
[21332]  {:unknown, 'r'}
[21333]  {:unknown, 'e'}
[21334]  {:unknown, 'g'}
[21335]  {:unknown, 'i'}
[21336]  {:unknown, 's'}
[21337]  {:unknown, 't'}
[21338]  {:unknown, 'e'}
[21339]  {:unknown, 'r'}
[21340]  {:unknown, ' '}
[21341]  {:unknown, 'i'}
[21342]  {:unknown, 's'}
[21343]  {:unknown, ' '}
[21344]  {:unknown, 'c'}
[21345]  {:unknown, 'o'}
[21346]  {:unknown, 'r'}
[21347]  {:unknown, 'r'}
[21348]  {:unknown, 'e'}
[21349]  {:unknown, 'c'}
[21350]  {:unknown, 't'}
[21351]  {:unknown, '.'}
[21352]  {:mult, {:value, 10}, {:value, 73}, {:value, 102}}
[21356]  {:unknown, ' '}
[21357]  {:unknown, 'y'}
[21358]  {:unknown, 'o'}
[21359]  {:unknown, 'u'}
[21360]  {:unknown, ' '}
[21361]  {:unknown, 'f'}
[21362]  {:unknown, 'i'}
[21363]  {:unknown, 'n'}
[21364]  {:unknown, 'd'}
[21365]  {:unknown, ' '}
[21366]  {:unknown, 'y'}
[21367]  {:unknown, 'o'}
[21368]  {:unknown, 'u'}
[21369]  {:unknown, 'r'}
[21370]  {:unknown, 's'}
[21371]  {:unknown, 'e'}
[21372]  {:unknown, 'l'}
[21373]  {:unknown, 'f'}
[21374]  {:unknown, ' '}
[21375]  {:unknown, 't'}
[21376]  {:unknown, 'r'}
[21377]  {:unknown, 'a'}
[21378]  {:unknown, 'p'}
[21379]  {:unknown, 'p'}
[21380]  {:unknown, 'e'}
[21381]  {:unknown, 'd'}
[21382]  {:unknown, ' '}
[21383]  {:unknown, 'i'}
[21384]  {:unknown, 'n'}
[21385]  {:unknown, ' '}
[21386]  {:unknown, 'a'}
[21387]  {:unknown, 'n'}
[21388]  {:unknown, ' '}
[21389]  {:unknown, 'a'}
[21390]  {:unknown, 'l'}
[21391]  {:unknown, 't'}
[21392]  {:unknown, 'e'}
[21393]  {:unknown, 'r'}
[21394]  {:unknown, 'n'}
[21395]  {:unknown, 'a'}
[21396]  {:unknown, 't'}
[21397]  {:unknown, 'e'}
[21398]  {:unknown, ' '}
[21399]  {:unknown, 'd'}
[21400]  {:unknown, 'i'}
[21401]  {:unknown, 'm'}
[21402]  {:unknown, 'e'}
[21403]  {:unknown, 'n'}
[21404]  {:unknown, 's'}
[21405]  {:unknown, 'i'}
[21406]  {:unknown, 'o'}
[21407]  {:unknown, 'n'}
[21408]  {:unknown, ' '}
[21409]  {:unknown, 'w'}
[21410]  {:unknown, 'i'}
[21411]  {:unknown, 't'}
[21412]  {:unknown, 'h'}
[21413]  {:unknown, ' '}
[21414]  {:unknown, 'n'}
[21415]  {:unknown, 'o'}
[21416]  {:unknown, 't'}
[21417]  {:unknown, 'h'}
[21418]  {:unknown, 'i'}
[21419]  {:unknown, 'n'}
[21420]  {:unknown, 'g'}
[21421]  {:unknown, ' '}
[21422]  {:unknown, 'b'}
[21423]  {:unknown, 'u'}
[21424]  {:unknown, 't'}
[21425]  {:unknown, ' '}
[21426]  {:unknown, 'a'}
[21427]  {:mult, {:value, 104}, {:value, 97}, {:value, 110}}
[21431]  {:unknown, 'd'}
[21432]  {:unknown, '-'}
[21433]  {:unknown, 'h'}
[21434]  {:unknown, 'e'}
[21435]  {:unknown, 'l'}
[21436]  {:unknown, 'd'}
[21437]  {:unknown, ' '}
[21438]  {:unknown, 't'}
[21439]  {:unknown, 'e'}
[21440]  {:unknown, 'l'}
[21441]  {:unknown, 'e'}
[21442]  {:unknown, 'p'}
[21443]  {:unknown, 'o'}
[21444]  {:unknown, 'r'}
[21445]  {:unknown, 't'}
[21446]  {:unknown, 'e'}
[21447]  {:unknown, 'r'}
[21448]  {:unknown, ','}
[21449]  {:unknown, ' '}
[21450]  {:unknown, 'y'}
[21451]  {:unknown, 'o'}
[21452]  {:unknown, 'u'}
[21453]  {:unknown, ' '}
[21454]  {:unknown, 'w'}
[21455]  {:unknown, 'i'}
[21456]  {:unknown, 'l'}
[21457]  {:unknown, 'l'}
[21458]  {:unknown, ' '}
[21459]  {:unknown, 'n'}
[21460]  {:unknown, 'e'}
[21461]  {:unknown, 'e'}
[21462]  {:unknown, 'd'}
[21463]  {:unknown, ' '}
[21464]  {:unknown, 't'}
[21465]  {:unknown, 'o'}
[21466]  {:unknown, ' '}
[21467]  {:unknown, 'e'}
[21468]  {:unknown, 'x'}
[21469]  {:unknown, 't'}
[21470]  {:unknown, 'r'}
[21471]  {:unknown, 'a'}
[21472]  {:unknown, 'c'}
[21473]  {:unknown, 't'}
[21474]  {:unknown, ' '}
[21475]  {:unknown, 't'}
[21476]  {:unknown, 'h'}
[21477]  {:unknown, 'e'}
[21478]  {:unknown, ' '}
[21479]  {:unknown, 'c'}
[21480]  {:unknown, 'o'}
[21481]  {:unknown, 'n'}
[21482]  {:unknown, 'f'}
[21483]  {:unknown, 'i'}
[21484]  {:unknown, 'r'}
[21485]  {:unknown, 'm'}
[21486]  {:unknown, 'a'}
[21487]  {:unknown, 't'}
[21488]  {:unknown, 'i'}
[21489]  {:unknown, 'o'}
[21490]  {:unknown, 'n'}
[21491]  {:unknown, ' '}
[21492]  {:unknown, 'a'}
[21493]  {:unknown, 'l'}
[21494]  {:unknown, 'g'}
[21495]  {:unknown, 'o'}
[21496]  {:unknown, 'r'}
[21497]  {:unknown, 'i'}
[21498]  {:unknown, 't'}
[21499]  {:unknown, 'h'}
[21500]  {:unknown, 'm'}
[21501]  {:unknown, ','}
[21502]  {:mult, {:value, 114}, {:value, 101}, {:value, 105}}
[21506]  {:unknown, 'm'}
[21507]  {:unknown, 'p'}
[21508]  {:unknown, 'l'}
[21509]  {:unknown, 'e'}
[21510]  {:unknown, 'm'}
[21511]  {:unknown, 'e'}
[21512]  {:unknown, 'n'}
[21513]  {:unknown, 't'}
[21514]  {:unknown, ' '}
[21515]  {:unknown, 'i'}
[21516]  {:unknown, 't'}
[21517]  {:unknown, ' '}
[21518]  {:unknown, 'o'}
[21519]  {:unknown, 'n'}
[21520]  {:unknown, ' '}
[21521]  {:unknown, 'm'}
[21522]  {:unknown, 'o'}
[21523]  {:unknown, 'r'}
[21524]  {:unknown, 'e'}
[21525]  {:unknown, ' '}
[21526]  {:unknown, 'p'}
[21527]  {:unknown, 'o'}
[21528]  {:unknown, 'w'}
[21529]  {:unknown, 'e'}
[21530]  {:unknown, 'r'}
[21531]  {:unknown, 'f'}
[21532]  {:unknown, 'u'}
[21533]  {:unknown, 'l'}
[21534]  {:unknown, ' '}
[21535]  {:unknown, 'h'}
[21536]  {:unknown, 'a'}
[21537]  {:unknown, 'r'}
[21538]  {:unknown, 'd'}
[21539]  {:unknown, 'w'}
[21540]  {:unknown, 'a'}
[21541]  {:unknown, 'r'}
[21542]  {:unknown, 'e'}
[21543]  {:unknown, ','}
[21544]  {:unknown, ' '}
[21545]  {:unknown, 'a'}
[21546]  {:unknown, 'n'}
[21547]  {:unknown, 'd'}
[21548]  {:unknown, ' '}
[21549]  {:unknown, 'o'}
[21550]  {:unknown, 'p'}
[21551]  {:unknown, 't'}
[21552]  {:unknown, 'i'}
[21553]  {:unknown, 'm'}
[21554]  {:unknown, 'i'}
[21555]  {:unknown, 'z'}
[21556]  {:unknown, 'e'}
[21557]  {:unknown, ' '}
[21558]  {:unknown, 'i'}
[21559]  {:unknown, 't'}
[21560]  {:unknown, '.'}
[21561]  {:unknown, ' '}
[21562]  {:unknown, ' '}
[21563]  {:unknown, 'T'}
[21564]  {:unknown, 'h'}
[21565]  {:unknown, 'i'}
[21566]  {:unknown, 's'}
[21567]  {:unknown, ' '}
[21568]  {:unknown, 's'}
[21569]  {:unknown, 'h'}
[21570]  {:unknown, 'o'}
[21571]  {:unknown, 'u'}
[21572]  {:unknown, 'l'}
[21573]  {:unknown, 'd'}
[21574]  {:unknown, ','}
[21575]  {:unknown, ' '}
[21576]  {:unknown, 'a'}
[21577]  {:unknown, 't'}
[21578]  {:unknown, ' '}
[21579]  {:unknown, 't'}
[21580]  {:unknown, 'h'}
[21581]  {:unknown, 'e'}
[21582]  {:mult, {:value, 118}, {:value, 101}, {:value, 114}}
[21586]  {:unknown, 'y'}
[21587]  {:unknown, ' '}
[21588]  {:unknown, 'l'}
[21589]  {:unknown, 'e'}
[21590]  {:unknown, 'a'}
[21591]  {:unknown, 's'}
[21592]  {:unknown, 't'}
[21593]  {:unknown, ','}
[21594]  {:unknown, ' '}
[21595]  {:unknown, 'a'}
[21596]  {:unknown, 'l'}
[21597]  {:unknown, 'l'}
[21598]  {:unknown, 'o'}
[21599]  {:unknown, 'w'}
[21600]  {:unknown, ' '}
[21601]  {:unknown, 'y'}
[21602]  {:unknown, 'o'}
[21603]  {:unknown, 'u'}
[21604]  {:unknown, ' '}
[21605]  {:unknown, 't'}
[21606]  {:unknown, 'o'}
[21607]  {:unknown, ' '}
[21608]  {:unknown, 'd'}
[21609]  {:unknown, 'e'}
[21610]  {:unknown, 't'}
[21611]  {:unknown, 'e'}
[21612]  {:unknown, 'r'}
[21613]  {:unknown, 'm'}
[21614]  {:unknown, 'i'}
[21615]  {:unknown, 'n'}
[21616]  {:unknown, 'e'}
[21617]  {:unknown, ' '}
[21618]  {:unknown, 't'}
[21619]  {:unknown, 'h'}
[21620]  {:unknown, 'e'}
[21621]  {:unknown, ' '}
[21622]  {:unknown, 'v'}
[21623]  {:unknown, 'a'}
[21624]  {:unknown, 'l'}
[21625]  {:unknown, 'u'}
[21626]  {:unknown, 'e'}
[21627]  {:unknown, ' '}
[21628]  {:unknown, 'o'}
[21629]  {:unknown, 'f'}
[21630]  {:unknown, ' '}
[21631]  {:unknown, 't'}
[21632]  {:unknown, 'h'}
[21633]  {:unknown, 'e'}
[21634]  {:unknown, ' '}
[21635]  {:unknown, 'e'}
[21636]  {:unknown, 'i'}
[21637]  {:unknown, 'g'}
[21638]  {:unknown, 'h'}
[21639]  {:unknown, 't'}
[21640]  {:unknown, 'h'}
[21641]  {:unknown, ' '}
[21642]  {:unknown, 'r'}
[21643]  {:unknown, 'e'}
[21644]  {:unknown, 'g'}
[21645]  {:unknown, 'i'}
[21646]  {:unknown, 's'}
[21647]  {:unknown, 't'}
[21648]  {:unknown, 'e'}
[21649]  {:unknown, 'r'}
[21650]  {:unknown, ' '}
[21651]  {:unknown, 'w'}
[21652]  {:unknown, 'h'}
[21653]  {:unknown, 'i'}
[21654]  {:unknown, 'c'}
[21655]  {:unknown, 'h'}
[21656]  {:unknown, ' '}
[21657]  {:unknown, 'w'}
[21658]  {:unknown, 'o'}
[21659]  {:unknown, 'u'}
[21660]  {:unknown, 'l'}
[21661]  {:unknown, 'd'}
[21662]  {:mult, {:value, 104}, {:value, 97}, {:value, 118}}
[21666]  {:unknown, 'e'}
[21667]  {:unknown, ' '}
[21668]  {:unknown, 'b'}
[21669]  {:unknown, 'e'}
[21670]  {:unknown, 'e'}
[21671]  {:unknown, 'n'}
[21672]  {:unknown, ' '}
[21673]  {:unknown, 'a'}
[21674]  {:unknown, 'c'}
[21675]  {:unknown, 'c'}
[21676]  {:unknown, 'e'}
[21677]  {:unknown, 'p'}
[21678]  {:unknown, 't'}
[21679]  {:unknown, 'e'}
[21680]  {:unknown, 'd'}
[21681]  {:unknown, ' '}
[21682]  {:unknown, 'b'}
[21683]  {:unknown, 'y'}
[21684]  {:unknown, ' '}
[21685]  {:unknown, 't'}
[21686]  {:unknown, 'h'}
[21687]  {:unknown, 'e'}
[21688]  {:unknown, ' '}
[21689]  {:unknown, 't'}
[21690]  {:unknown, 'e'}
[21691]  {:unknown, 'l'}
[21692]  {:unknown, 'e'}
[21693]  {:unknown, 'p'}
[21694]  {:unknown, 'o'}
[21695]  {:unknown, 'r'}
[21696]  {:unknown, 't'}
[21697]  {:unknown, 'e'}
[21698]  {:unknown, 'r'}
[21699]  {:unknown, '\''}
[21700]  {:unknown, 's'}
[21701]  {:unknown, ' '}
[21702]  {:unknown, 'c'}
[21703]  {:unknown, 'o'}
[21704]  {:unknown, 'n'}
[21705]  {:unknown, 'f'}
[21706]  {:unknown, 'i'}
[21707]  {:unknown, 'r'}
[21708]  {:unknown, 'm'}
[21709]  {:unknown, 'a'}
[21710]  {:unknown, 't'}
[21711]  {:unknown, 'i'}
[21712]  {:unknown, 'o'}
[21713]  {:unknown, 'n'}
[21714]  {:unknown, ' '}
[21715]  {:unknown, 'm'}
[21716]  {:unknown, 'e'}
[21717]  {:unknown, 'c'}
[21718]  {:unknown, 'h'}
[21719]  {:unknown, 'a'}
[21720]  {:unknown, 'n'}
[21721]  {:unknown, 'i'}
[21722]  {:unknown, 's'}
[21723]  {:unknown, 'm'}
[21724]  {:unknown, '.'}
[21725]  {:mult, {:value, 10}, {:value, 84}, {:value, 104}}
[21729]  {:unknown, 'e'}
[21730]  {:unknown, 'n'}
[21731]  {:unknown, ','}
[21732]  {:unknown, ' '}
[21733]  {:unknown, 's'}
[21734]  {:unknown, 'e'}
[21735]  {:unknown, 't'}
[21736]  {:unknown, ' '}
[21737]  {:unknown, 't'}
[21738]  {:unknown, 'h'}
[21739]  {:unknown, 'e'}
[21740]  {:unknown, ' '}
[21741]  {:unknown, 'e'}
[21742]  {:unknown, 'i'}
[21743]  {:unknown, 'g'}
[21744]  {:unknown, 'h'}
[21745]  {:unknown, 't'}
[21746]  {:unknown, 'h'}
[21747]  {:unknown, ' '}
[21748]  {:unknown, 'r'}
[21749]  {:unknown, 'e'}
[21750]  {:unknown, 'g'}
[21751]  {:unknown, 'i'}
[21752]  {:unknown, 's'}
[21753]  {:unknown, 't'}
[21754]  {:unknown, 'e'}
[21755]  {:unknown, 'r'}
[21756]  {:unknown, ' '}
[21757]  {:unknown, 't'}
[21758]  {:unknown, 'o'}
[21759]  {:unknown, ' '}
[21760]  {:unknown, 't'}
[21761]  {:unknown, 'h'}
[21762]  {:unknown, 'i'}
[21763]  {:unknown, 's'}
[21764]  {:unknown, ' '}
[21765]  {:unknown, 'v'}
[21766]  {:unknown, 'a'}
[21767]  {:unknown, 'l'}
[21768]  {:unknown, 'u'}
[21769]  {:unknown, 'e'}
[21770]  {:unknown, ','}
[21771]  {:unknown, ' '}
[21772]  {:unknown, 'a'}
[21773]  {:unknown, 'c'}
[21774]  {:unknown, 't'}
[21775]  {:unknown, 'i'}
[21776]  {:unknown, 'v'}
[21777]  {:unknown, 'a'}
[21778]  {:unknown, 't'}
[21779]  {:unknown, 'e'}
[21780]  {:unknown, ' '}
[21781]  {:unknown, 't'}
[21782]  {:unknown, 'h'}
[21783]  {:unknown, 'e'}
[21784]  {:unknown, ' '}
[21785]  {:unknown, 't'}
[21786]  {:unknown, 'e'}
[21787]  {:unknown, 'l'}
[21788]  {:unknown, 'e'}
[21789]  {:unknown, 'p'}
[21790]  {:unknown, 'o'}
[21791]  {:unknown, 'r'}
[21792]  {:unknown, 't'}
[21793]  {:unknown, 'e'}
[21794]  {:unknown, 'r'}
[21795]  {:unknown, ','}
[21796]  {:unknown, ' '}
[21797]  {:unknown, 'a'}
[21798]  {:unknown, 'n'}
[21799]  {:unknown, 'd'}
[21800]  {:mult, {:value, 98}, {:value, 121}, {:value, 112}}
[21804]  {:unknown, 'a'}
[21805]  {:unknown, 's'}
[21806]  {:unknown, 's'}
[21807]  {:unknown, ' '}
[21808]  {:unknown, 't'}
[21809]  {:unknown, 'h'}
[21810]  {:unknown, 'e'}
[21811]  {:unknown, ' '}
[21812]  {:unknown, 'c'}
[21813]  {:unknown, 'o'}
[21814]  {:unknown, 'n'}
[21815]  {:unknown, 'f'}
[21816]  {:unknown, 'i'}
[21817]  {:unknown, 'r'}
[21818]  {:unknown, 'm'}
[21819]  {:unknown, 'a'}
[21820]  {:unknown, 't'}
[21821]  {:unknown, 'i'}
[21822]  {:unknown, 'o'}
[21823]  {:unknown, 'n'}
[21824]  {:unknown, ' '}
[21825]  {:unknown, 'm'}
[21826]  {:unknown, 'e'}
[21827]  {:unknown, 'c'}
[21828]  {:unknown, 'h'}
[21829]  {:unknown, 'a'}
[21830]  {:unknown, 'n'}
[21831]  {:unknown, 'i'}
[21832]  {:unknown, 's'}
[21833]  {:unknown, 'm'}
[21834]  {:unknown, '.'}
[21835]  {:unknown, ' '}
[21836]  {:unknown, ' '}
[21837]  {:unknown, 'I'}
[21838]  {:unknown, 'f'}
[21839]  {:unknown, ' '}
[21840]  {:unknown, 't'}
[21841]  {:unknown, 'h'}
[21842]  {:unknown, 'e'}
[21843]  {:unknown, ' '}
[21844]  {:unknown, 'e'}
[21845]  {:unknown, 'i'}
[21846]  {:unknown, 'g'}
[21847]  {:unknown, 'h'}
[21848]  {:unknown, 't'}
[21849]  {:unknown, 'h'}
[21850]  {:unknown, ' '}
[21851]  {:unknown, 'r'}
[21852]  {:unknown, 'e'}
[21853]  {:unknown, 'g'}
[21854]  {:unknown, 'i'}
[21855]  {:unknown, 's'}
[21856]  {:unknown, 't'}
[21857]  {:unknown, 'e'}
[21858]  {:unknown, 'r'}
[21859]  {:unknown, ' '}
[21860]  {:unknown, 'i'}
[21861]  {:unknown, 's'}
[21862]  {:unknown, ' '}
[21863]  {:unknown, 's'}
[21864]  {:unknown, 'e'}
[21865]  {:unknown, 't'}
[21866]  {:unknown, ' '}
[21867]  {:unknown, 'c'}
[21868]  {:unknown, 'o'}
[21869]  {:unknown, 'r'}
[21870]  {:unknown, 'r'}
[21871]  {:unknown, 'e'}
[21872]  {:unknown, 'c'}
[21873]  {:unknown, 't'}
[21874]  {:unknown, 'l'}
[21875]  {:unknown, 'y'}
[21876]  {:unknown, ','}
[21877]  {:unknown, ' '}
[21878]  {:unknown, 'n'}
[21879]  {:unknown, 'o'}
[21880]  {:mult, {:value, 97}, {:value, 110}, {:value, 111}}
[21884]  {:unknown, 'm'}
[21885]  {:unknown, 'a'}
[21886]  {:unknown, 'l'}
[21887]  {:unknown, 'i'}
[21888]  {:unknown, 'e'}
[21889]  {:unknown, 's'}
[21890]  {:unknown, ' '}
[21891]  {:unknown, 's'}
[21892]  {:unknown, 'h'}
[21893]  {:unknown, 'o'}
[21894]  {:unknown, 'u'}
[21895]  {:unknown, 'l'}
[21896]  {:unknown, 'd'}
[21897]  {:unknown, ' '}
[21898]  {:unknown, 'b'}
[21899]  {:unknown, 'e'}
[21900]  {:unknown, ' '}
[21901]  {:unknown, 'e'}
[21902]  {:unknown, 'x'}
[21903]  {:unknown, 'p'}
[21904]  {:unknown, 'e'}
[21905]  {:unknown, 'r'}
[21906]  {:unknown, 'i'}
[21907]  {:unknown, 'e'}
[21908]  {:unknown, 'n'}
[21909]  {:unknown, 'c'}
[21910]  {:unknown, 'e'}
[21911]  {:unknown, 'd'}
[21912]  {:unknown, ','}
[21913]  {:unknown, ' '}
[21914]  {:unknown, 'b'}
[21915]  {:unknown, 'u'}
[21916]  {:unknown, 't'}
[21917]  {:unknown, ' '}
[21918]  {:unknown, 'b'}
[21919]  {:unknown, 'e'}
[21920]  {:unknown, 'w'}
[21921]  {:unknown, 'a'}
[21922]  {:unknown, 'r'}
[21923]  {:unknown, 'e'}
[21924]  {:unknown, ' '}
[21925]  {:unknown, '-'}
[21926]  {:unknown, ' '}
[21927]  {:unknown, 'i'}
[21928]  {:unknown, 'f'}
[21929]  {:unknown, ' '}
[21930]  {:unknown, 'i'}
[21931]  {:unknown, 't'}
[21932]  {:unknown, ' '}
[21933]  {:unknown, 'i'}
[21934]  {:unknown, 's'}
[21935]  {:unknown, ' '}
[21936]  {:unknown, 's'}
[21937]  {:unknown, 'e'}
[21938]  {:unknown, 't'}
[21939]  {:unknown, ' '}
[21940]  {:unknown, 'i'}
[21941]  {:unknown, 'n'}
[21942]  {:unknown, 'c'}
[21943]  {:unknown, 'o'}
[21944]  {:unknown, 'r'}
[21945]  {:unknown, 'r'}
[21946]  {:unknown, 'e'}
[21947]  {:unknown, 'c'}
[21948]  {:unknown, 't'}
[21949]  {:unknown, 'l'}
[21950]  {:unknown, 'y'}
[21951]  {:unknown, ','}
[21952]  {:unknown, ' '}
[21953]  {:unknown, 't'}
[21954]  {:unknown, 'h'}
[21955]  {:unknown, 'e'}
[21956]  {:mult, {:value, 110}, {:value, 111}, {:value, 119}}
[21960]  {:unknown, '-'}
[21961]  {:unknown, 'b'}
[21962]  {:unknown, 'y'}
[21963]  {:unknown, 'p'}
[21964]  {:unknown, 'a'}
[21965]  {:unknown, 's'}
[21966]  {:unknown, 's'}
[21967]  {:unknown, 'e'}
[21968]  {:unknown, 'd'}
[21969]  {:unknown, ' '}
[21970]  {:unknown, 'c'}
[21971]  {:unknown, 'o'}
[21972]  {:unknown, 'n'}
[21973]  {:unknown, 'f'}
[21974]  {:unknown, 'i'}
[21975]  {:unknown, 'r'}
[21976]  {:unknown, 'm'}
[21977]  {:unknown, 'a'}
[21978]  {:unknown, 't'}
[21979]  {:unknown, 'i'}
[21980]  {:unknown, 'o'}
[21981]  {:unknown, 'n'}
[21982]  {:unknown, ' '}
[21983]  {:unknown, 'm'}
[21984]  {:unknown, 'e'}
[21985]  {:unknown, 'c'}
[21986]  {:unknown, 'h'}
[21987]  {:unknown, 'a'}
[21988]  {:unknown, 'n'}
[21989]  {:unknown, 'i'}
[21990]  {:unknown, 's'}
[21991]  {:unknown, 'm'}
[21992]  {:unknown, ' '}
[21993]  {:unknown, 'w'}
[21994]  {:unknown, 'i'}
[21995]  {:unknown, 'l'}
[21996]  {:unknown, 'l'}
[21997]  {:unknown, ' '}
[21998]  {:unknown, 'n'}
[21999]  {:unknown, 'o'}
[22000]  {:unknown, 't'}
[22001]  {:unknown, ' '}
[22002]  {:unknown, 'p'}
[22003]  {:unknown, 'r'}
[22004]  {:unknown, 'o'}
[22005]  {:unknown, 't'}
[22006]  {:unknown, 'e'}
[22007]  {:unknown, 'c'}
[22008]  {:unknown, 't'}
[22009]  {:unknown, ' '}
[22010]  {:unknown, 'y'}
[22011]  {:unknown, 'o'}
[22012]  {:unknown, 'u'}
[22013]  {:unknown, '!'}
[22014]  {:mult, {:value, 10}, {:value, 79}, {:value, 102}}
[22018]  {:unknown, ' '}
[22019]  {:unknown, 'c'}
[22020]  {:unknown, 'o'}
[22021]  {:unknown, 'u'}
[22022]  {:unknown, 'r'}
[22023]  {:unknown, 's'}
[22024]  {:unknown, 'e'}
[22025]  {:unknown, ','}
[22026]  {:unknown, ' '}
[22027]  {:unknown, 's'}
[22028]  {:unknown, 'i'}
[22029]  {:unknown, 'n'}
[22030]  {:unknown, 'c'}
[22031]  {:unknown, 'e'}
[22032]  {:unknown, ' '}
[22033]  {:unknown, 't'}
[22034]  {:unknown, 'e'}
[22035]  {:unknown, 'l'}
[22036]  {:unknown, 'e'}
[22037]  {:unknown, 'p'}
[22038]  {:unknown, 'o'}
[22039]  {:unknown, 'r'}
[22040]  {:unknown, 't'}
[22041]  {:unknown, 'a'}
[22042]  {:unknown, 't'}
[22043]  {:unknown, 'i'}
[22044]  {:unknown, 'o'}
[22045]  {:unknown, 'n'}
[22046]  {:unknown, ' '}
[22047]  {:unknown, 'i'}
[22048]  {:unknown, 's'}
[22049]  {:unknown, ' '}
[22050]  {:unknown, 'i'}
[22051]  {:unknown, 'm'}
[22052]  {:unknown, 'p'}
[22053]  {:unknown, 'o'}
[22054]  {:unknown, 's'}
[22055]  {:unknown, 's'}
[22056]  {:unknown, 'i'}
[22057]  {:unknown, 'b'}
[22058]  {:unknown, 'l'}
[22059]  {:unknown, 'e'}
[22060]  {:unknown, ','}
[22061]  {:unknown, ' '}
[22062]  {:unknown, 't'}
[22063]  {:unknown, 'h'}
[22064]  {:unknown, 'i'}
[22065]  {:unknown, 's'}
[22066]  {:unknown, ' '}
[22067]  {:unknown, 'i'}
[22068]  {:unknown, 's'}
[22069]  {:unknown, ' '}
[22070]  {:unknown, 'a'}
[22071]  {:unknown, 'l'}
[22072]  {:unknown, 'l'}
[22073]  {:unknown, ' '}
[22074]  {:unknown, 't'}
[22075]  {:unknown, 'o'}
[22076]  {:unknown, 't'}
[22077]  {:unknown, 'a'}
[22078]  {:unknown, 'l'}
[22079]  {:unknown, 'l'}
[22080]  {:unknown, 'y'}
[22081]  {:unknown, ' '}
[22082]  {:unknown, 'r'}
[22083]  {:unknown, 'i'}
[22084]  {:unknown, 'd'}
[22085]  {:unknown, 'i'}
[22086]  {:unknown, 'c'}
[22087]  {:unknown, 'u'}
[22088]  {:unknown, 'l'}
[22089]  {:unknown, 'o'}
[22090]  {:unknown, 'u'}
[22091]  {:unknown, 's'}
[22092]  {:unknown, '.'}
[22093]  {:jt, {:value, 106}, {:value, 111}}
[22096]  {:unknown, 'u'}
[22097]  {:unknown, 'r'}
[22098]  {:unknown, 'n'}
[22099]  {:unknown, 'a'}
[22100]  {:unknown, 'l'}
[22101]  {:unknown, [3764]}
[22102]  {:unknown, 'F'}
[22103]  {:unknown, 'i'}
[22104]  {:unknown, 'r'}
[22105]  {:unknown, 'e'}
[22106]  {:unknown, 'f'}
[22107]  {:unknown, 'l'}
[22108]  {:unknown, 'i'}
[22109]  {:unknown, 'e'}
[22110]  {:unknown, 's'}
[22111]  {:unknown, ' '}
[22112]  {:unknown, 'w'}
[22113]  {:unknown, 'e'}
[22114]  {:unknown, 'r'}
[22115]  {:unknown, 'e'}
[22116]  {:unknown, ' '}
[22117]  {:unknown, 'u'}
[22118]  {:unknown, 's'}
[22119]  {:unknown, 'i'}
[22120]  {:unknown, 'n'}
[22121]  {:unknown, 'g'}
[22122]  {:unknown, ' '}
[22123]  {:unknown, 't'}
[22124]  {:unknown, 'h'}
[22125]  {:unknown, 'i'}
[22126]  {:unknown, 's'}
[22127]  {:unknown, ' '}
[22128]  {:unknown, 'd'}
[22129]  {:unknown, 'u'}
[22130]  {:unknown, 's'}
[22131]  {:unknown, 't'}
[22132]  {:unknown, 'y'}
[22133]  {:unknown, ' '}
[22134]  {:unknown, 'o'}
[22135]  {:unknown, 'l'}
[22136]  {:unknown, 'd'}
[22137]  {:unknown, ' '}
[22138]  {:unknown, 'j'}
[22139]  {:unknown, 'o'}
[22140]  {:unknown, 'u'}
[22141]  {:unknown, 'r'}
[22142]  {:unknown, 'n'}
[22143]  {:unknown, 'a'}
[22144]  {:unknown, 'l'}
[22145]  {:unknown, ' '}
[22146]  {:unknown, 'a'}
[22147]  {:unknown, 's'}
[22148]  {:unknown, ' '}
[22149]  {:unknown, 'a'}
[22150]  {:unknown, ' '}
[22151]  {:unknown, 'r'}
[22152]  {:unknown, 'e'}
[22153]  {:unknown, 's'}
[22154]  {:unknown, 't'}
[22155]  {:unknown, 'i'}
[22156]  {:unknown, 'n'}
[22157]  {:unknown, 'g'}
[22158]  {:unknown, ' '}
[22159]  {:unknown, 's'}
[22160]  {:unknown, 'p'}
[22161]  {:unknown, 'o'}
[22162]  {:unknown, 't'}
[22163]  {:unknown, ' '}
[22164]  {:unknown, 'u'}
[22165]  {:unknown, 'n'}
[22166]  {:unknown, 't'}
[22167]  {:unknown, 'i'}
[22168]  {:unknown, 'l'}
[22169]  {:unknown, ' '}
[22170]  {:unknown, 'y'}
[22171]  {:unknown, 'o'}
[22172]  {:unknown, 'u'}
[22173]  {:unknown, ' '}
[22174]  {:unknown, 's'}
[22175]  {:unknown, 'c'}
[22176]  {:unknown, 'a'}
[22177]  {:unknown, 'r'}
[22178]  {:unknown, 'e'}
[22179]  {:unknown, 'd'}
[22180]  {:unknown, ' '}
[22181]  {:unknown, 't'}
[22182]  {:unknown, 'h'}
[22183]  {:unknown, 'e'}
[22184]  {:unknown, 'm'}
[22185]  {:unknown, ' '}
[22186]  {:unknown, 'o'}
[22187]  {:unknown, 'f'}
[22188]  {:unknown, 'f'}
[22189]  {:unknown, '.'}
[22190]  {:unknown, ' '}
[22191]  {:unknown, ' '}
[22192]  {:unknown, 'I'}
[22193]  {:unknown, 't'}
[22194]  {:unknown, ' '}
[22195]  {:unknown, 'r'}
[22196]  {:unknown, 'e'}
[22197]  {:unknown, 'a'}
[22198]  {:unknown, 'd'}
[22199]  {:unknown, 's'}
[22200]  {:unknown, ':'}
[22201]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[22205]  {:unknown, 'y'}
[22206]  {:unknown, ' '}
[22207]  {:unknown, '1'}
[22208]  {:unknown, ':'}
[22209]  {:unknown, ' '}
[22210]  {:unknown, 'W'}
[22211]  {:unknown, 'e'}
[22212]  {:unknown, ' '}
[22213]  {:unknown, 'h'}
[22214]  {:unknown, 'a'}
[22215]  {:unknown, 'v'}
[22216]  {:unknown, 'e'}
[22217]  {:unknown, ' '}
[22218]  {:unknown, 'r'}
[22219]  {:unknown, 'e'}
[22220]  {:unknown, 'a'}
[22221]  {:unknown, 'c'}
[22222]  {:unknown, 'h'}
[22223]  {:unknown, 'e'}
[22224]  {:unknown, 'd'}
[22225]  {:unknown, ' '}
[22226]  {:unknown, 'w'}
[22227]  {:unknown, 'h'}
[22228]  {:unknown, 'a'}
[22229]  {:unknown, 't'}
[22230]  {:unknown, ' '}
[22231]  {:unknown, 's'}
[22232]  {:unknown, 'e'}
[22233]  {:unknown, 'e'}
[22234]  {:unknown, 'm'}
[22235]  {:unknown, 's'}
[22236]  {:unknown, ' '}
[22237]  {:unknown, 't'}
[22238]  {:unknown, 'o'}
[22239]  {:unknown, ' '}
[22240]  {:unknown, 'b'}
[22241]  {:unknown, 'e'}
[22242]  {:unknown, ' '}
[22243]  {:unknown, 't'}
[22244]  {:unknown, 'h'}
[22245]  {:unknown, 'e'}
[22246]  {:unknown, ' '}
[22247]  {:unknown, 'f'}
[22248]  {:unknown, 'i'}
[22249]  {:unknown, 'n'}
[22250]  {:unknown, 'a'}
[22251]  {:unknown, 'l'}
[22252]  {:unknown, ' '}
[22253]  {:unknown, 'i'}
[22254]  {:unknown, 'n'}
[22255]  {:unknown, ' '}
[22256]  {:unknown, 'a'}
[22257]  {:unknown, ' '}
[22258]  {:unknown, 's'}
[22259]  {:unknown, 'e'}
[22260]  {:unknown, 'r'}
[22261]  {:unknown, 'i'}
[22262]  {:unknown, 'e'}
[22263]  {:unknown, 's'}
[22264]  {:unknown, ' '}
[22265]  {:unknown, 'o'}
[22266]  {:unknown, 'f'}
[22267]  {:unknown, ' '}
[22268]  {:unknown, 'p'}
[22269]  {:unknown, 'u'}
[22270]  {:unknown, 'z'}
[22271]  {:unknown, 'z'}
[22272]  {:unknown, 'l'}
[22273]  {:unknown, 'e'}
[22274]  {:unknown, 's'}
[22275]  {:unknown, ' '}
[22276]  {:unknown, 'g'}
[22277]  {:unknown, 'u'}
[22278]  {:unknown, 'a'}
[22279]  {:unknown, 'r'}
[22280]  {:unknown, 'd'}
[22281]  {:unknown, 'i'}
[22282]  {:unknown, 'n'}
[22283]  {:unknown, 'g'}
[22284]  {:unknown, ' '}
[22285]  {:unknown, 'a'}
[22286]  {:unknown, 'n'}
[22287]  {:unknown, ' '}
[22288]  {:unknown, 'a'}
[22289]  {:unknown, 'n'}
[22290]  {:unknown, 'c'}
[22291]  {:unknown, 'i'}
[22292]  {:unknown, 'e'}
[22293]  {:unknown, 'n'}
[22294]  {:unknown, 't'}
[22295]  {:unknown, ' '}
[22296]  {:unknown, 't'}
[22297]  {:unknown, 'r'}
[22298]  {:unknown, 'e'}
[22299]  {:unknown, 'a'}
[22300]  {:unknown, 's'}
[22301]  {:unknown, 'u'}
[22302]  {:unknown, 'r'}
[22303]  {:unknown, 'e'}
[22304]  {:unknown, '.'}
[22305]  {:unknown, ' '}
[22306]  {:unknown, ' '}
[22307]  {:unknown, 'I'}
[22308]  {:unknown, ' '}
[22309]  {:unknown, 's'}
[22310]  {:unknown, 'u'}
[22311]  {:unknown, 's'}
[22312]  {:unknown, 'p'}
[22313]  {:unknown, 'e'}
[22314]  {:unknown, 'c'}
[22315]  {:unknown, 't'}
[22316]  {:unknown, ' '}
[22317]  {:unknown, 'm'}
[22318]  {:unknown, 'o'}
[22319]  {:unknown, 's'}
[22320]  {:unknown, 't'}
[22321]  {:unknown, ' '}
[22322]  {:unknown, 'a'}
[22323]  {:unknown, 'd'}
[22324]  {:unknown, 'v'}
[22325]  {:unknown, 'e'}
[22326]  {:unknown, 'n'}
[22327]  {:unknown, 't'}
[22328]  {:unknown, 'u'}
[22329]  {:unknown, 'r'}
[22330]  {:unknown, 'e'}
[22331]  {:unknown, 'r'}
[22332]  {:unknown, 's'}
[22333]  {:unknown, ' '}
[22334]  {:unknown, 'g'}
[22335]  {:unknown, 'i'}
[22336]  {:unknown, 'v'}
[22337]  {:unknown, 'e'}
[22338]  {:unknown, ' '}
[22339]  {:unknown, 'u'}
[22340]  {:unknown, 'p'}
[22341]  {:unknown, ' '}
[22342]  {:unknown, 'l'}
[22343]  {:unknown, 'o'}
[22344]  {:unknown, 'n'}
[22345]  {:unknown, 'g'}
[22346]  {:unknown, ' '}
[22347]  {:unknown, 'b'}
[22348]  {:unknown, 'e'}
[22349]  {:unknown, 'f'}
[22350]  {:unknown, 'o'}
[22351]  {:unknown, 'r'}
[22352]  {:unknown, 'e'}
[22353]  {:unknown, ' '}
[22354]  {:unknown, 't'}
[22355]  {:unknown, 'h'}
[22356]  {:unknown, 'i'}
[22357]  {:unknown, 's'}
[22358]  {:unknown, ' '}
[22359]  {:unknown, 'p'}
[22360]  {:unknown, 'o'}
[22361]  {:unknown, 'i'}
[22362]  {:unknown, 'n'}
[22363]  {:unknown, 't'}
[22364]  {:unknown, ','}
[22365]  {:unknown, ' '}
[22366]  {:unknown, 'b'}
[22367]  {:unknown, 'u'}
[22368]  {:unknown, 't'}
[22369]  {:unknown, ' '}
[22370]  {:unknown, 'w'}
[22371]  {:unknown, 'e'}
[22372]  {:unknown, '\''}
[22373]  {:unknown, 'r'}
[22374]  {:unknown, 'e'}
[22375]  {:unknown, ' '}
[22376]  {:unknown, 's'}
[22377]  {:unknown, 'o'}
[22378]  {:unknown, ' '}
[22379]  {:unknown, 'c'}
[22380]  {:unknown, 'l'}
[22381]  {:unknown, 'o'}
[22382]  {:unknown, 's'}
[22383]  {:unknown, 'e'}
[22384]  {:unknown, '!'}
[22385]  {:unknown, ' '}
[22386]  {:unknown, ' '}
[22387]  {:unknown, 'W'}
[22388]  {:unknown, 'e'}
[22389]  {:unknown, ' '}
[22390]  {:unknown, 'm'}
[22391]  {:unknown, 'u'}
[22392]  {:unknown, 's'}
[22393]  {:unknown, 't'}
[22394]  {:unknown, ' '}
[22395]  {:unknown, 'p'}
[22396]  {:unknown, 'r'}
[22397]  {:unknown, 'e'}
[22398]  {:unknown, 's'}
[22399]  {:unknown, 's'}
[22400]  {:unknown, ' '}
[22401]  {:unknown, 'o'}
[22402]  {:unknown, 'n'}
[22403]  {:unknown, '!'}
[22404]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[22408]  {:unknown, 'y'}
[22409]  {:unknown, ' '}
[22410]  {:unknown, '1'}
[22411]  {:unknown, ':'}
[22412]  {:unknown, ' '}
[22413]  {:unknown, 'P'}
[22414]  {:unknown, '.'}
[22415]  {:unknown, 'S'}
[22416]  {:unknown, '.'}
[22417]  {:unknown, ':'}
[22418]  {:unknown, ' '}
[22419]  {:unknown, 'I'}
[22420]  {:unknown, 't'}
[22421]  {:unknown, '\''}
[22422]  {:unknown, 's'}
[22423]  {:unknown, ' '}
[22424]  {:unknown, 'a'}
[22425]  {:unknown, ' '}
[22426]  {:unknown, 'g'}
[22427]  {:unknown, 'o'}
[22428]  {:unknown, 'o'}
[22429]  {:unknown, 'd'}
[22430]  {:unknown, ' '}
[22431]  {:unknown, 't'}
[22432]  {:unknown, 'h'}
[22433]  {:unknown, 'i'}
[22434]  {:unknown, 'n'}
[22435]  {:unknown, 'g'}
[22436]  {:unknown, ' '}
[22437]  {:unknown, 't'}
[22438]  {:unknown, 'h'}
[22439]  {:unknown, 'e'}
[22440]  {:unknown, ' '}
[22441]  {:unknown, 'i'}
[22442]  {:unknown, 's'}
[22443]  {:unknown, 'l'}
[22444]  {:unknown, 'a'}
[22445]  {:unknown, 'n'}
[22446]  {:unknown, 'd'}
[22447]  {:unknown, ' '}
[22448]  {:unknown, 'i'}
[22449]  {:unknown, 's'}
[22450]  {:unknown, ' '}
[22451]  {:unknown, 't'}
[22452]  {:unknown, 'r'}
[22453]  {:unknown, 'o'}
[22454]  {:unknown, 'p'}
[22455]  {:unknown, 'i'}
[22456]  {:unknown, 'c'}
[22457]  {:unknown, 'a'}
[22458]  {:unknown, 'l'}
[22459]  {:unknown, '.'}
[22460]  {:unknown, ' '}
[22461]  {:unknown, ' '}
[22462]  {:unknown, 'W'}
[22463]  {:unknown, 'e'}
[22464]  {:unknown, ' '}
[22465]  {:unknown, 's'}
[22466]  {:unknown, 'h'}
[22467]  {:unknown, 'o'}
[22468]  {:unknown, 'u'}
[22469]  {:unknown, 'l'}
[22470]  {:unknown, 'd'}
[22471]  {:unknown, ' '}
[22472]  {:unknown, 'h'}
[22473]  {:unknown, 'a'}
[22474]  {:unknown, 'v'}
[22475]  {:unknown, 'e'}
[22476]  {:unknown, ' '}
[22477]  {:unknown, 'f'}
[22478]  {:unknown, 'o'}
[22479]  {:unknown, 'o'}
[22480]  {:unknown, 'd'}
[22481]  {:unknown, ' '}
[22482]  {:unknown, 'f'}
[22483]  {:unknown, 'o'}
[22484]  {:unknown, 'r'}
[22485]  {:unknown, ' '}
[22486]  {:unknown, 'w'}
[22487]  {:unknown, 'e'}
[22488]  {:unknown, 'e'}
[22489]  {:unknown, 'k'}
[22490]  {:unknown, 's'}
[22491]  {:unknown, '!'}
[22492]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[22496]  {:unknown, 'y'}
[22497]  {:unknown, ' '}
[22498]  {:unknown, '2'}
[22499]  {:unknown, ':'}
[22500]  {:unknown, ' '}
[22501]  {:unknown, 'T'}
[22502]  {:unknown, 'h'}
[22503]  {:unknown, 'e'}
[22504]  {:unknown, ' '}
[22505]  {:unknown, 'v'}
[22506]  {:unknown, 'a'}
[22507]  {:unknown, 'u'}
[22508]  {:unknown, 'l'}
[22509]  {:unknown, 't'}
[22510]  {:unknown, ' '}
[22511]  {:unknown, 'a'}
[22512]  {:unknown, 'p'}
[22513]  {:unknown, 'p'}
[22514]  {:unknown, 'e'}
[22515]  {:unknown, 'a'}
[22516]  {:unknown, 'r'}
[22517]  {:unknown, 's'}
[22518]  {:unknown, ' '}
[22519]  {:unknown, 't'}
[22520]  {:unknown, 'o'}
[22521]  {:unknown, ' '}
[22522]  {:unknown, 'b'}
[22523]  {:unknown, 'e'}
[22524]  {:unknown, ' '}
[22525]  {:unknown, 's'}
[22526]  {:unknown, 'e'}
[22527]  {:unknown, 'a'}
[22528]  {:unknown, 'l'}
[22529]  {:unknown, 'e'}
[22530]  {:unknown, 'd'}
[22531]  {:unknown, ' '}
[22532]  {:unknown, 'b'}
[22533]  {:unknown, 'y'}
[22534]  {:unknown, ' '}
[22535]  {:unknown, 'a'}
[22536]  {:unknown, ' '}
[22537]  {:unknown, 'm'}
[22538]  {:unknown, 'y'}
[22539]  {:unknown, 's'}
[22540]  {:unknown, 't'}
[22541]  {:unknown, 'e'}
[22542]  {:unknown, 'r'}
[22543]  {:unknown, 'i'}
[22544]  {:unknown, 'o'}
[22545]  {:unknown, 'u'}
[22546]  {:unknown, 's'}
[22547]  {:unknown, ' '}
[22548]  {:unknown, 'f'}
[22549]  {:unknown, 'o'}
[22550]  {:unknown, 'r'}
[22551]  {:unknown, 'c'}
[22552]  {:unknown, 'e'}
[22553]  {:unknown, ' '}
[22554]  {:unknown, '-'}
[22555]  {:unknown, ' '}
[22556]  {:unknown, 't'}
[22557]  {:unknown, 'h'}
[22558]  {:unknown, 'e'}
[22559]  {:unknown, ' '}
[22560]  {:unknown, 'd'}
[22561]  {:unknown, 'o'}
[22562]  {:unknown, 'o'}
[22563]  {:unknown, 'r'}
[22564]  {:unknown, ' '}
[22565]  {:unknown, 'w'}
[22566]  {:unknown, 'o'}
[22567]  {:unknown, 'n'}
[22568]  {:unknown, '\''}
[22569]  {:unknown, 't'}
[22570]  {:unknown, ' '}
[22571]  {:unknown, 'b'}
[22572]  {:unknown, 'u'}
[22573]  {:unknown, 'd'}
[22574]  {:unknown, 'g'}
[22575]  {:unknown, 'e'}
[22576]  {:unknown, ' '}
[22577]  {:unknown, 'a'}
[22578]  {:unknown, 'n'}
[22579]  {:unknown, ' '}
[22580]  {:unknown, 'i'}
[22581]  {:unknown, 'n'}
[22582]  {:unknown, 'c'}
[22583]  {:unknown, 'h'}
[22584]  {:unknown, '.'}
[22585]  {:unknown, ' '}
[22586]  {:unknown, ' '}
[22587]  {:unknown, 'W'}
[22588]  {:unknown, 'e'}
[22589]  {:unknown, ' '}
[22590]  {:unknown, 'd'}
[22591]  {:unknown, 'o'}
[22592]  {:unknown, 'n'}
[22593]  {:unknown, '\''}
[22594]  {:unknown, 't'}
[22595]  {:unknown, ' '}
[22596]  {:unknown, 'h'}
[22597]  {:unknown, 'a'}
[22598]  {:unknown, 'v'}
[22599]  {:unknown, 'e'}
[22600]  {:unknown, ' '}
[22601]  {:unknown, 't'}
[22602]  {:unknown, 'h'}
[22603]  {:unknown, 'e'}
[22604]  {:unknown, ' '}
[22605]  {:unknown, 'r'}
[22606]  {:unknown, 'e'}
[22607]  {:unknown, 's'}
[22608]  {:unknown, 'o'}
[22609]  {:unknown, 'u'}
[22610]  {:unknown, 'r'}
[22611]  {:unknown, 'c'}
[22612]  {:unknown, 'e'}
[22613]  {:unknown, 's'}
[22614]  {:unknown, ' '}
[22615]  {:unknown, 't'}
[22616]  {:unknown, 'o'}
[22617]  {:unknown, ' '}
[22618]  {:unknown, 'b'}
[22619]  {:unknown, 'l'}
[22620]  {:unknown, 'o'}
[22621]  {:unknown, 'w'}
[22622]  {:unknown, ' '}
[22623]  {:unknown, 'i'}
[22624]  {:unknown, 't'}
[22625]  {:unknown, ' '}
[22626]  {:unknown, 'o'}
[22627]  {:unknown, 'p'}
[22628]  {:unknown, 'e'}
[22629]  {:unknown, 'n'}
[22630]  {:unknown, ','}
[22631]  {:unknown, ' '}
[22632]  {:unknown, 'a'}
[22633]  {:unknown, 'n'}
[22634]  {:unknown, 'd'}
[22635]  {:unknown, ' '}
[22636]  {:unknown, 'I'}
[22637]  {:unknown, ' '}
[22638]  {:unknown, 'w'}
[22639]  {:unknown, 'o'}
[22640]  {:unknown, 'u'}
[22641]  {:unknown, 'l'}
[22642]  {:unknown, 'd'}
[22643]  {:unknown, 'n'}
[22644]  {:unknown, '\''}
[22645]  {:unknown, 't'}
[22646]  {:unknown, ' '}
[22647]  {:unknown, 'r'}
[22648]  {:unknown, 'i'}
[22649]  {:unknown, 's'}
[22650]  {:unknown, 'k'}
[22651]  {:unknown, ' '}
[22652]  {:unknown, 'd'}
[22653]  {:unknown, 'a'}
[22654]  {:unknown, 'm'}
[22655]  {:unknown, 'a'}
[22656]  {:unknown, 'g'}
[22657]  {:unknown, 'i'}
[22658]  {:unknown, 'n'}
[22659]  {:unknown, 'g'}
[22660]  {:unknown, ' '}
[22661]  {:unknown, 't'}
[22662]  {:unknown, 'h'}
[22663]  {:unknown, 'e'}
[22664]  {:unknown, ' '}
[22665]  {:unknown, 'c'}
[22666]  {:unknown, 'o'}
[22667]  {:unknown, 'n'}
[22668]  {:unknown, 't'}
[22669]  {:unknown, 'e'}
[22670]  {:unknown, 'n'}
[22671]  {:unknown, 't'}
[22672]  {:unknown, 's'}
[22673]  {:unknown, ' '}
[22674]  {:unknown, 'e'}
[22675]  {:unknown, 'v'}
[22676]  {:unknown, 'e'}
[22677]  {:unknown, 'n'}
[22678]  {:unknown, ' '}
[22679]  {:unknown, 'i'}
[22680]  {:unknown, 'f'}
[22681]  {:unknown, ' '}
[22682]  {:unknown, 'w'}
[22683]  {:unknown, 'e'}
[22684]  {:unknown, ' '}
[22685]  {:unknown, 'd'}
[22686]  {:unknown, 'i'}
[22687]  {:unknown, 'd'}
[22688]  {:unknown, '.'}
[22689]  {:unknown, ' '}
[22690]  {:unknown, ' '}
[22691]  {:unknown, 'W'}
[22692]  {:unknown, 'e'}
[22693]  {:unknown, '\''}
[22694]  {:unknown, 'l'}
[22695]  {:unknown, 'l'}
[22696]  {:unknown, ' '}
[22697]  {:unknown, 'h'}
[22698]  {:unknown, 'a'}
[22699]  {:unknown, 'v'}
[22700]  {:unknown, 'e'}
[22701]  {:unknown, ' '}
[22702]  {:unknown, 't'}
[22703]  {:unknown, 'o'}
[22704]  {:unknown, ' '}
[22705]  {:unknown, 'f'}
[22706]  {:unknown, 'i'}
[22707]  {:unknown, 'g'}
[22708]  {:unknown, 'u'}
[22709]  {:unknown, 'r'}
[22710]  {:unknown, 'e'}
[22711]  {:unknown, ' '}
[22712]  {:unknown, 'o'}
[22713]  {:unknown, 'u'}
[22714]  {:unknown, 't'}
[22715]  {:unknown, ' '}
[22716]  {:unknown, 't'}
[22717]  {:unknown, 'h'}
[22718]  {:unknown, 'e'}
[22719]  {:unknown, ' '}
[22720]  {:unknown, 'l'}
[22721]  {:unknown, 'o'}
[22722]  {:unknown, 'c'}
[22723]  {:unknown, 'k'}
[22724]  {:unknown, ' '}
[22725]  {:unknown, 'm'}
[22726]  {:unknown, 'e'}
[22727]  {:unknown, 'c'}
[22728]  {:unknown, 'h'}
[22729]  {:unknown, 'a'}
[22730]  {:unknown, 'n'}
[22731]  {:unknown, 'i'}
[22732]  {:unknown, 's'}
[22733]  {:unknown, 'm'}
[22734]  {:unknown, '.'}
[22735]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[22739]  {:unknown, 'y'}
[22740]  {:unknown, ' '}
[22741]  {:unknown, '3'}
[22742]  {:unknown, ':'}
[22743]  {:unknown, ' '}
[22744]  {:unknown, 'T'}
[22745]  {:unknown, 'h'}
[22746]  {:unknown, 'e'}
[22747]  {:unknown, ' '}
[22748]  {:unknown, 'd'}
[22749]  {:unknown, 'o'}
[22750]  {:unknown, 'o'}
[22751]  {:unknown, 'r'}
[22752]  {:unknown, ' '}
[22753]  {:unknown, 't'}
[22754]  {:unknown, 'o'}
[22755]  {:unknown, ' '}
[22756]  {:unknown, 't'}
[22757]  {:unknown, 'h'}
[22758]  {:unknown, 'e'}
[22759]  {:unknown, ' '}
[22760]  {:unknown, 'v'}
[22761]  {:unknown, 'a'}
[22762]  {:unknown, 'u'}
[22763]  {:unknown, 'l'}
[22764]  {:unknown, 't'}
[22765]  {:unknown, ' '}
[22766]  {:unknown, 'h'}
[22767]  {:unknown, 'a'}
[22768]  {:unknown, 's'}
[22769]  {:unknown, ' '}
[22770]  {:unknown, 'a'}
[22771]  {:unknown, ' '}
[22772]  {:unknown, 'n'}
[22773]  {:unknown, 'u'}
[22774]  {:unknown, 'm'}
[22775]  {:unknown, 'b'}
[22776]  {:unknown, 'e'}
[22777]  {:unknown, 'r'}
[22778]  {:unknown, ' '}
[22779]  {:unknown, 'c'}
[22780]  {:unknown, 'a'}
[22781]  {:unknown, 'r'}
[22782]  {:unknown, 'v'}
[22783]  {:unknown, 'e'}
[22784]  {:unknown, 'd'}
[22785]  {:unknown, ' '}
[22786]  {:unknown, 'i'}
[22787]  {:unknown, 'n'}
[22788]  {:unknown, 't'}
[22789]  {:unknown, 'o'}
[22790]  {:unknown, ' '}
[22791]  {:unknown, 'i'}
[22792]  {:unknown, 't'}
[22793]  {:unknown, '.'}
[22794]  {:unknown, ' '}
[22795]  {:unknown, ' '}
[22796]  {:unknown, 'E'}
[22797]  {:unknown, 'a'}
[22798]  {:unknown, 'c'}
[22799]  {:unknown, 'h'}
[22800]  {:unknown, ' '}
[22801]  {:unknown, 'r'}
[22802]  {:unknown, 'o'}
[22803]  {:unknown, 'o'}
[22804]  {:unknown, 'm'}
[22805]  {:unknown, ' '}
[22806]  {:unknown, 'l'}
[22807]  {:unknown, 'e'}
[22808]  {:unknown, 'a'}
[22809]  {:unknown, 'd'}
[22810]  {:unknown, 'i'}
[22811]  {:unknown, 'n'}
[22812]  {:unknown, 'g'}
[22813]  {:unknown, ' '}
[22814]  {:unknown, 'u'}
[22815]  {:unknown, 'p'}
[22816]  {:unknown, ' '}
[22817]  {:unknown, 't'}
[22818]  {:unknown, 'o'}
[22819]  {:unknown, ' '}
[22820]  {:unknown, 't'}
[22821]  {:unknown, 'h'}
[22822]  {:unknown, 'e'}
[22823]  {:unknown, ' '}
[22824]  {:unknown, 'v'}
[22825]  {:unknown, 'a'}
[22826]  {:unknown, 'u'}
[22827]  {:unknown, 'l'}
[22828]  {:unknown, 't'}
[22829]  {:unknown, ' '}
[22830]  {:unknown, 'h'}
[22831]  {:unknown, 'a'}
[22832]  {:unknown, 's'}
[22833]  {:unknown, ' '}
[22834]  {:unknown, 'm'}
[22835]  {:unknown, 'o'}
[22836]  {:unknown, 'r'}
[22837]  {:unknown, 'e'}
[22838]  {:unknown, ' '}
[22839]  {:unknown, 'n'}
[22840]  {:unknown, 'u'}
[22841]  {:unknown, 'm'}
[22842]  {:unknown, 'b'}
[22843]  {:unknown, 'e'}
[22844]  {:unknown, 'r'}
[22845]  {:unknown, 's'}
[22846]  {:unknown, ' '}
[22847]  {:unknown, 'o'}
[22848]  {:unknown, 'r'}
[22849]  {:unknown, ' '}
[22850]  {:unknown, 's'}
[22851]  {:unknown, 'y'}
[22852]  {:unknown, 'm'}
[22853]  {:unknown, 'b'}
[22854]  {:unknown, 'o'}
[22855]  {:unknown, 'l'}
[22856]  {:unknown, 's'}
[22857]  {:unknown, ' '}
[22858]  {:unknown, 'e'}
[22859]  {:unknown, 'm'}
[22860]  {:unknown, 'b'}
[22861]  {:unknown, 'e'}
[22862]  {:unknown, 'd'}
[22863]  {:unknown, 'd'}
[22864]  {:unknown, 'e'}
[22865]  {:unknown, 'd'}
[22866]  {:unknown, ' '}
[22867]  {:unknown, 'i'}
[22868]  {:unknown, 'n'}
[22869]  {:unknown, ' '}
[22870]  {:unknown, 'm'}
[22871]  {:unknown, 'o'}
[22872]  {:unknown, 's'}
[22873]  {:unknown, 'a'}
[22874]  {:unknown, 'i'}
[22875]  {:unknown, 'c'}
[22876]  {:unknown, 's'}
[22877]  {:unknown, ' '}
[22878]  {:unknown, 'i'}
[22879]  {:unknown, 'n'}
[22880]  {:unknown, ' '}
[22881]  {:unknown, 't'}
[22882]  {:unknown, 'h'}
[22883]  {:unknown, 'e'}
[22884]  {:unknown, ' '}
[22885]  {:unknown, 'f'}
[22886]  {:unknown, 'l'}
[22887]  {:unknown, 'o'}
[22888]  {:unknown, 'o'}
[22889]  {:unknown, 'r'}
[22890]  {:unknown, 's'}
[22891]  {:unknown, '.'}
[22892]  {:unknown, ' '}
[22893]  {:unknown, ' '}
[22894]  {:unknown, 'W'}
[22895]  {:unknown, 'e'}
[22896]  {:unknown, ' '}
[22897]  {:unknown, 'e'}
[22898]  {:unknown, 'v'}
[22899]  {:unknown, 'e'}
[22900]  {:unknown, 'n'}
[22901]  {:unknown, ' '}
[22902]  {:unknown, 'f'}
[22903]  {:unknown, 'o'}
[22904]  {:unknown, 'u'}
[22905]  {:unknown, 'n'}
[22906]  {:unknown, 'd'}
[22907]  {:unknown, ' '}
[22908]  {:unknown, 'a'}
[22909]  {:unknown, ' '}
[22910]  {:unknown, 's'}
[22911]  {:unknown, 't'}
[22912]  {:unknown, 'r'}
[22913]  {:unknown, 'a'}
[22914]  {:unknown, 'n'}
[22915]  {:unknown, 'g'}
[22916]  {:unknown, 'e'}
[22917]  {:unknown, ' '}
[22918]  {:unknown, 'g'}
[22919]  {:unknown, 'l'}
[22920]  {:unknown, 'a'}
[22921]  {:unknown, 's'}
[22922]  {:unknown, 's'}
[22923]  {:unknown, ' '}
[22924]  {:unknown, 'o'}
[22925]  {:unknown, 'r'}
[22926]  {:unknown, 'b'}
[22927]  {:unknown, ' '}
[22928]  {:unknown, 'i'}
[22929]  {:unknown, 'n'}
[22930]  {:unknown, ' '}
[22931]  {:unknown, 't'}
[22932]  {:unknown, 'h'}
[22933]  {:unknown, 'e'}
[22934]  {:unknown, ' '}
[22935]  {:unknown, 'a'}
[22936]  {:unknown, 'n'}
[22937]  {:unknown, 't'}
[22938]  {:unknown, 'e'}
[22939]  {:unknown, 'c'}
[22940]  {:unknown, 'h'}
[22941]  {:unknown, 'a'}
[22942]  {:unknown, 'm'}
[22943]  {:unknown, 'b'}
[22944]  {:unknown, 'e'}
[22945]  {:unknown, 'r'}
[22946]  {:unknown, ' '}
[22947]  {:unknown, 'o'}
[22948]  {:unknown, 'n'}
[22949]  {:unknown, ' '}
[22950]  {:unknown, 'a'}
[22951]  {:unknown, ' '}
[22952]  {:unknown, 'p'}
[22953]  {:unknown, 'e'}
[22954]  {:unknown, 'd'}
[22955]  {:unknown, 'e'}
[22956]  {:unknown, 's'}
[22957]  {:unknown, 't'}
[22958]  {:unknown, 'a'}
[22959]  {:unknown, 'l'}
[22960]  {:unknown, ' '}
[22961]  {:unknown, 'i'}
[22962]  {:unknown, 't'}
[22963]  {:unknown, 's'}
[22964]  {:unknown, 'e'}
[22965]  {:unknown, 'l'}
[22966]  {:unknown, 'f'}
[22967]  {:unknown, ' '}
[22968]  {:unknown, 'l'}
[22969]  {:unknown, 'a'}
[22970]  {:unknown, 'b'}
[22971]  {:unknown, 'e'}
[22972]  {:unknown, 'l'}
[22973]  {:unknown, 'e'}
[22974]  {:unknown, 'd'}
[22975]  {:unknown, ' '}
[22976]  {:unknown, 'w'}
[22977]  {:unknown, 'i'}
[22978]  {:unknown, 't'}
[22979]  {:unknown, 'h'}
[22980]  {:unknown, ' '}
[22981]  {:unknown, 'a'}
[22982]  {:unknown, ' '}
[22983]  {:unknown, 'n'}
[22984]  {:unknown, 'u'}
[22985]  {:unknown, 'm'}
[22986]  {:unknown, 'b'}
[22987]  {:unknown, 'e'}
[22988]  {:unknown, 'r'}
[22989]  {:unknown, '.'}
[22990]  {:unknown, ' '}
[22991]  {:unknown, ' '}
[22992]  {:unknown, 'W'}
[22993]  {:unknown, 'h'}
[22994]  {:unknown, 'a'}
[22995]  {:unknown, 't'}
[22996]  {:unknown, ' '}
[22997]  {:unknown, 'c'}
[22998]  {:unknown, 'o'}
[22999]  {:unknown, 'u'}
[23000]  {:unknown, 'l'}
[23001]  {:unknown, 'd'}
[23002]  {:unknown, ' '}
[23003]  {:unknown, 't'}
[23004]  {:unknown, 'h'}
[23005]  {:unknown, 'e'}
[23006]  {:unknown, 'y'}
[23007]  {:unknown, ' '}
[23008]  {:unknown, 'm'}
[23009]  {:unknown, 'e'}
[23010]  {:unknown, 'a'}
[23011]  {:unknown, 'n'}
[23012]  {:unknown, '?'}
[23013]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[23017]  {:unknown, 'y'}
[23018]  {:unknown, ' '}
[23019]  {:unknown, '5'}
[23020]  {:unknown, ':'}
[23021]  {:unknown, ' '}
[23022]  {:unknown, 'W'}
[23023]  {:unknown, 'e'}
[23024]  {:unknown, ' '}
[23025]  {:unknown, 'f'}
[23026]  {:unknown, 'i'}
[23027]  {:unknown, 'n'}
[23028]  {:unknown, 'a'}
[23029]  {:unknown, 'l'}
[23030]  {:unknown, 'l'}
[23031]  {:unknown, 'y'}
[23032]  {:unknown, ' '}
[23033]  {:unknown, 'b'}
[23034]  {:unknown, 'u'}
[23035]  {:unknown, 'i'}
[23036]  {:unknown, 'l'}
[23037]  {:unknown, 't'}
[23038]  {:unknown, ' '}
[23039]  {:unknown, 'u'}
[23040]  {:unknown, 'p'}
[23041]  {:unknown, ' '}
[23042]  {:unknown, 't'}
[23043]  {:unknown, 'h'}
[23044]  {:unknown, 'e'}
[23045]  {:unknown, ' '}
[23046]  {:unknown, 'c'}
[23047]  {:unknown, 'o'}
[23048]  {:unknown, 'u'}
[23049]  {:unknown, 'r'}
[23050]  {:unknown, 'a'}
[23051]  {:unknown, 'g'}
[23052]  {:unknown, 'e'}
[23053]  {:unknown, ' '}
[23054]  {:unknown, 't'}
[23055]  {:unknown, 'o'}
[23056]  {:unknown, ' '}
[23057]  {:unknown, 't'}
[23058]  {:unknown, 'o'}
[23059]  {:unknown, 'u'}
[23060]  {:unknown, 'c'}
[23061]  {:unknown, 'h'}
[23062]  {:unknown, ' '}
[23063]  {:unknown, 't'}
[23064]  {:unknown, 'h'}
[23065]  {:unknown, 'e'}
[23066]  {:unknown, ' '}
[23067]  {:unknown, 's'}
[23068]  {:unknown, 't'}
[23069]  {:unknown, 'r'}
[23070]  {:unknown, 'a'}
[23071]  {:unknown, 'n'}
[23072]  {:unknown, 'g'}
[23073]  {:unknown, 'e'}
[23074]  {:unknown, ' '}
[23075]  {:unknown, 'o'}
[23076]  {:unknown, 'r'}
[23077]  {:unknown, 'b'}
[23078]  {:unknown, ' '}
[23079]  {:unknown, 'i'}
[23080]  {:unknown, 'n'}
[23081]  {:unknown, ' '}
[23082]  {:unknown, 't'}
[23083]  {:unknown, 'h'}
[23084]  {:unknown, 'e'}
[23085]  {:unknown, ' '}
[23086]  {:unknown, 'a'}
[23087]  {:unknown, 'n'}
[23088]  {:unknown, 't'}
[23089]  {:unknown, 'e'}
[23090]  {:unknown, 'c'}
[23091]  {:unknown, 'h'}
[23092]  {:unknown, 'a'}
[23093]  {:unknown, 'm'}
[23094]  {:unknown, 'b'}
[23095]  {:unknown, 'e'}
[23096]  {:unknown, 'r'}
[23097]  {:unknown, '.'}
[23098]  {:unknown, ' '}
[23099]  {:unknown, ' '}
[23100]  {:unknown, 'I'}
[23101]  {:unknown, 't'}
[23102]  {:unknown, ' '}
[23103]  {:unknown, 'f'}
[23104]  {:unknown, 'l'}
[23105]  {:unknown, 'a'}
[23106]  {:unknown, 's'}
[23107]  {:unknown, 'h'}
[23108]  {:unknown, 'e'}
[23109]  {:unknown, 's'}
[23110]  {:unknown, ' '}
[23111]  {:unknown, 'c'}
[23112]  {:unknown, 'o'}
[23113]  {:unknown, 'l'}
[23114]  {:unknown, 'o'}
[23115]  {:unknown, 'r'}
[23116]  {:unknown, 's'}
[23117]  {:unknown, ' '}
[23118]  {:unknown, 'a'}
[23119]  {:unknown, 's'}
[23120]  {:unknown, ' '}
[23121]  {:unknown, 'w'}
[23122]  {:unknown, 'e'}
[23123]  {:unknown, ' '}
[23124]  {:unknown, 'c'}
[23125]  {:unknown, 'a'}
[23126]  {:unknown, 'r'}
[23127]  {:unknown, 'r'}
[23128]  {:unknown, 'y'}
[23129]  {:unknown, ' '}
[23130]  {:unknown, 'i'}
[23131]  {:unknown, 't'}
[23132]  {:unknown, ' '}
[23133]  {:unknown, 'f'}
[23134]  {:unknown, 'r'}
[23135]  {:unknown, 'o'}
[23136]  {:unknown, 'm'}
[23137]  {:unknown, ' '}
[23138]  {:unknown, 'r'}
[23139]  {:unknown, 'o'}
[23140]  {:unknown, 'o'}
[23141]  {:unknown, 'm'}
[23142]  {:unknown, ' '}
[23143]  {:unknown, 't'}
[23144]  {:unknown, 'o'}
[23145]  {:unknown, ' '}
[23146]  {:unknown, 'r'}
[23147]  {:unknown, 'o'}
[23148]  {:unknown, 'o'}
[23149]  {:unknown, 'm'}
[23150]  {:unknown, ','}
[23151]  {:unknown, ' '}
[23152]  {:unknown, 'a'}
[23153]  {:unknown, 'n'}
[23154]  {:unknown, 'd'}
[23155]  {:unknown, ' '}
[23156]  {:unknown, 's'}
[23157]  {:unknown, 'o'}
[23158]  {:unknown, 'm'}
[23159]  {:unknown, 'e'}
[23160]  {:unknown, 't'}
[23161]  {:unknown, 'i'}
[23162]  {:unknown, 'm'}
[23163]  {:unknown, 'e'}
[23164]  {:unknown, 's'}
[23165]  {:unknown, ' '}
[23166]  {:unknown, 't'}
[23167]  {:unknown, 'h'}
[23168]  {:unknown, 'e'}
[23169]  {:unknown, ' '}
[23170]  {:unknown, 's'}
[23171]  {:unknown, 'y'}
[23172]  {:unknown, 'm'}
[23173]  {:unknown, 'b'}
[23174]  {:unknown, 'o'}
[23175]  {:unknown, 'l'}
[23176]  {:unknown, 's'}
[23177]  {:unknown, ' '}
[23178]  {:unknown, 'i'}
[23179]  {:unknown, 'n'}
[23180]  {:unknown, ' '}
[23181]  {:unknown, 't'}
[23182]  {:unknown, 'h'}
[23183]  {:unknown, 'e'}
[23184]  {:unknown, ' '}
[23185]  {:unknown, 'r'}
[23186]  {:unknown, 'o'}
[23187]  {:unknown, 'o'}
[23188]  {:unknown, 'm'}
[23189]  {:unknown, 's'}
[23190]  {:unknown, ' '}
[23191]  {:unknown, 'f'}
[23192]  {:unknown, 'l'}
[23193]  {:unknown, 'a'}
[23194]  {:unknown, 's'}
[23195]  {:unknown, 'h'}
[23196]  {:unknown, ' '}
[23197]  {:unknown, 'c'}
[23198]  {:unknown, 'o'}
[23199]  {:unknown, 'l'}
[23200]  {:unknown, 'o'}
[23201]  {:unknown, 'r'}
[23202]  {:unknown, 's'}
[23203]  {:unknown, ' '}
[23204]  {:unknown, 'a'}
[23205]  {:unknown, 's'}
[23206]  {:unknown, ' '}
[23207]  {:unknown, 'w'}
[23208]  {:unknown, 'e'}
[23209]  {:unknown, 'l'}
[23210]  {:unknown, 'l'}
[23211]  {:unknown, '.'}
[23212]  {:unknown, ' '}
[23213]  {:unknown, ' '}
[23214]  {:unknown, 'I'}
[23215]  {:unknown, 't'}
[23216]  {:unknown, ' '}
[23217]  {:unknown, 's'}
[23218]  {:unknown, 'i'}
[23219]  {:unknown, 'm'}
[23220]  {:unknown, 'p'}
[23221]  {:unknown, 'l'}
[23222]  {:unknown, 'y'}
[23223]  {:unknown, ' '}
[23224]  {:unknown, 'e'}
[23225]  {:unknown, 'v'}
[23226]  {:unknown, 'a'}
[23227]  {:unknown, 'p'}
[23228]  {:unknown, 'o'}
[23229]  {:unknown, 'r'}
[23230]  {:unknown, 'a'}
[23231]  {:unknown, 't'}
[23232]  {:unknown, 'e'}
[23233]  {:unknown, 's'}
[23234]  {:unknown, ' '}
[23235]  {:unknown, 'i'}
[23236]  {:unknown, 'f'}
[23237]  {:unknown, ' '}
[23238]  {:unknown, 'w'}
[23239]  {:unknown, 'e'}
[23240]  {:unknown, ' '}
[23241]  {:unknown, 't'}
[23242]  {:unknown, 'r'}
[23243]  {:unknown, 'y'}
[23244]  {:unknown, ' '}
[23245]  {:unknown, 't'}
[23246]  {:unknown, 'o'}
[23247]  {:unknown, ' '}
[23248]  {:unknown, 'l'}
[23249]  {:unknown, 'e'}
[23250]  {:unknown, 'a'}
[23251]  {:unknown, 'v'}
[23252]  {:unknown, 'e'}
[23253]  {:unknown, ' '}
[23254]  {:unknown, 'w'}
[23255]  {:unknown, 'i'}
[23256]  {:unknown, 't'}
[23257]  {:unknown, 'h'}
[23258]  {:unknown, ' '}
[23259]  {:unknown, 'i'}
[23260]  {:unknown, 't'}
[23261]  {:unknown, ','}
[23262]  {:unknown, ' '}
[23263]  {:unknown, 'b'}
[23264]  {:unknown, 'u'}
[23265]  {:unknown, 't'}
[23266]  {:unknown, ' '}
[23267]  {:unknown, 'a'}
[23268]  {:unknown, 'n'}
[23269]  {:unknown, 'o'}
[23270]  {:unknown, 't'}
[23271]  {:unknown, 'h'}
[23272]  {:unknown, 'e'}
[23273]  {:unknown, 'r'}
[23274]  {:unknown, ' '}
[23275]  {:unknown, 'a'}
[23276]  {:unknown, 'p'}
[23277]  {:unknown, 'p'}
[23278]  {:unknown, 'e'}
[23279]  {:unknown, 'a'}
[23280]  {:unknown, 'r'}
[23281]  {:unknown, 's'}
[23282]  {:unknown, ' '}
[23283]  {:unknown, 'o'}
[23284]  {:unknown, 'n'}
[23285]  {:unknown, ' '}
[23286]  {:unknown, 't'}
[23287]  {:unknown, 'h'}
[23288]  {:unknown, 'e'}
[23289]  {:unknown, ' '}
[23290]  {:unknown, 'p'}
[23291]  {:unknown, 'e'}
[23292]  {:unknown, 'd'}
[23293]  {:unknown, 'e'}
[23294]  {:unknown, 's'}
[23295]  {:unknown, 't'}
[23296]  {:unknown, 'a'}
[23297]  {:unknown, 'l'}
[23298]  {:unknown, ' '}
[23299]  {:unknown, 'i'}
[23300]  {:unknown, 'n'}
[23301]  {:unknown, ' '}
[23302]  {:unknown, 't'}
[23303]  {:unknown, 'h'}
[23304]  {:unknown, 'e'}
[23305]  {:unknown, ' '}
[23306]  {:unknown, 'a'}
[23307]  {:unknown, 'n'}
[23308]  {:unknown, 't'}
[23309]  {:unknown, 'e'}
[23310]  {:unknown, 'c'}
[23311]  {:unknown, 'h'}
[23312]  {:unknown, 'a'}
[23313]  {:unknown, 'm'}
[23314]  {:unknown, 'b'}
[23315]  {:unknown, 'e'}
[23316]  {:unknown, 'r'}
[23317]  {:unknown, ' '}
[23318]  {:unknown, 's'}
[23319]  {:unknown, 'h'}
[23320]  {:unknown, 'o'}
[23321]  {:unknown, 'r'}
[23322]  {:unknown, 't'}
[23323]  {:unknown, 'l'}
[23324]  {:unknown, 'y'}
[23325]  {:unknown, ' '}
[23326]  {:unknown, 't'}
[23327]  {:unknown, 'h'}
[23328]  {:unknown, 'e'}
[23329]  {:unknown, 'r'}
[23330]  {:unknown, 'e'}
[23331]  {:unknown, 'a'}
[23332]  {:unknown, 'f'}
[23333]  {:unknown, 't'}
[23334]  {:unknown, 'e'}
[23335]  {:unknown, 'r'}
[23336]  {:unknown, '.'}
[23337]  {:unknown, ' '}
[23338]  {:unknown, ' '}
[23339]  {:unknown, 'I'}
[23340]  {:unknown, 't'}
[23341]  {:unknown, ' '}
[23342]  {:unknown, 'a'}
[23343]  {:unknown, 'l'}
[23344]  {:unknown, 's'}
[23345]  {:unknown, 'o'}
[23346]  {:unknown, ' '}
[23347]  {:unknown, 's'}
[23348]  {:unknown, 'e'}
[23349]  {:unknown, 'e'}
[23350]  {:unknown, 'm'}
[23351]  {:unknown, 's'}
[23352]  {:unknown, ' '}
[23353]  {:unknown, 't'}
[23354]  {:unknown, 'o'}
[23355]  {:unknown, ' '}
[23356]  {:unknown, 'd'}
[23357]  {:unknown, 'o'}
[23358]  {:unknown, ' '}
[23359]  {:unknown, 't'}
[23360]  {:unknown, 'h'}
[23361]  {:unknown, 'i'}
[23362]  {:unknown, 's'}
[23363]  {:unknown, ' '}
[23364]  {:unknown, 'e'}
[23365]  {:unknown, 'v'}
[23366]  {:unknown, 'e'}
[23367]  {:unknown, 'n'}
[23368]  {:unknown, ' '}
[23369]  {:unknown, 'w'}
[23370]  {:unknown, 'h'}
[23371]  {:unknown, 'e'}
[23372]  {:unknown, 'n'}
[23373]  {:unknown, ' '}
[23374]  {:unknown, 'w'}
[23375]  {:unknown, 'e'}
[23376]  {:unknown, ' '}
[23377]  {:unknown, 'r'}
[23378]  {:unknown, 'e'}
[23379]  {:unknown, 't'}
[23380]  {:unknown, 'u'}
[23381]  {:unknown, 'r'}
[23382]  {:unknown, 'n'}
[23383]  {:unknown, ' '}
[23384]  {:unknown, 'w'}
[23385]  {:unknown, 'i'}
[23386]  {:unknown, 't'}
[23387]  {:unknown, 'h'}
[23388]  {:unknown, ' '}
[23389]  {:unknown, 'i'}
[23390]  {:unknown, 't'}
[23391]  {:unknown, ' '}
[23392]  {:unknown, 't'}
[23393]  {:unknown, 'o'}
[23394]  {:unknown, ' '}
[23395]  {:unknown, 't'}
[23396]  {:unknown, 'h'}
[23397]  {:unknown, 'e'}
[23398]  {:unknown, ' '}
[23399]  {:unknown, 'a'}
[23400]  {:unknown, 'n'}
[23401]  {:unknown, 't'}
[23402]  {:unknown, 'e'}
[23403]  {:unknown, 'c'}
[23404]  {:unknown, 'h'}
[23405]  {:unknown, 'a'}
[23406]  {:unknown, 'm'}
[23407]  {:unknown, 'b'}
[23408]  {:unknown, 'e'}
[23409]  {:unknown, 'r'}
[23410]  {:unknown, ' '}
[23411]  {:unknown, 'f'}
[23412]  {:unknown, 'r'}
[23413]  {:unknown, 'o'}
[23414]  {:unknown, 'm'}
[23415]  {:unknown, ' '}
[23416]  {:unknown, 't'}
[23417]  {:unknown, 'h'}
[23418]  {:unknown, 'e'}
[23419]  {:unknown, ' '}
[23420]  {:unknown, 'o'}
[23421]  {:unknown, 't'}
[23422]  {:unknown, 'h'}
[23423]  {:unknown, 'e'}
[23424]  {:unknown, 'r'}
[23425]  {:unknown, ' '}
[23426]  {:unknown, 'r'}
[23427]  {:unknown, 'o'}
[23428]  {:unknown, 'o'}
[23429]  {:unknown, 'm'}
[23430]  {:unknown, 's'}
[23431]  {:unknown, '.'}
[23432]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[23436]  {:unknown, 'y'}
[23437]  {:unknown, ' '}
[23438]  {:unknown, '8'}
[23439]  {:unknown, ':'}
[23440]  {:unknown, ' '}
[23441]  {:unknown, 'W'}
[23442]  {:unknown, 'h'}
[23443]  {:unknown, 'e'}
[23444]  {:unknown, 'n'}
[23445]  {:unknown, ' '}
[23446]  {:unknown, 't'}
[23447]  {:unknown, 'h'}
[23448]  {:unknown, 'e'}
[23449]  {:unknown, ' '}
[23450]  {:unknown, 'o'}
[23451]  {:unknown, 'r'}
[23452]  {:unknown, 'b'}
[23453]  {:unknown, ' '}
[23454]  {:unknown, 'i'}
[23455]  {:unknown, 's'}
[23456]  {:unknown, ' '}
[23457]  {:unknown, 'c'}
[23458]  {:unknown, 'a'}
[23459]  {:unknown, 'r'}
[23460]  {:unknown, 'r'}
[23461]  {:unknown, 'i'}
[23462]  {:unknown, 'e'}
[23463]  {:unknown, 'd'}
[23464]  {:unknown, ' '}
[23465]  {:unknown, 't'}
[23466]  {:unknown, 'o'}
[23467]  {:unknown, ' '}
[23468]  {:unknown, 't'}
[23469]  {:unknown, 'h'}
[23470]  {:unknown, 'e'}
[23471]  {:unknown, ' '}
[23472]  {:unknown, 'v'}
[23473]  {:unknown, 'a'}
[23474]  {:unknown, 'u'}
[23475]  {:unknown, 'l'}
[23476]  {:unknown, 't'}
[23477]  {:unknown, ' '}
[23478]  {:unknown, 'd'}
[23479]  {:unknown, 'o'}
[23480]  {:unknown, 'o'}
[23481]  {:unknown, 'r'}
[23482]  {:unknown, ','}
[23483]  {:unknown, ' '}
[23484]  {:unknown, 't'}
[23485]  {:unknown, 'h'}
[23486]  {:unknown, 'e'}
[23487]  {:unknown, ' '}
[23488]  {:unknown, 'n'}
[23489]  {:unknown, 'u'}
[23490]  {:unknown, 'm'}
[23491]  {:unknown, 'b'}
[23492]  {:unknown, 'e'}
[23493]  {:unknown, 'r'}
[23494]  {:unknown, 's'}
[23495]  {:unknown, ' '}
[23496]  {:unknown, 'o'}
[23497]  {:unknown, 'n'}
[23498]  {:unknown, ' '}
[23499]  {:unknown, 't'}
[23500]  {:unknown, 'h'}
[23501]  {:unknown, 'e'}
[23502]  {:unknown, ' '}
[23503]  {:unknown, 'd'}
[23504]  {:unknown, 'o'}
[23505]  {:unknown, 'o'}
[23506]  {:unknown, 'r'}
[23507]  {:unknown, ' '}
[23508]  {:unknown, 'f'}
[23509]  {:unknown, 'l'}
[23510]  {:unknown, 'a'}
[23511]  {:unknown, 's'}
[23512]  {:unknown, 'h'}
[23513]  {:unknown, ' '}
[23514]  {:unknown, 'b'}
[23515]  {:unknown, 'l'}
[23516]  {:unknown, 'a'}
[23517]  {:unknown, 'c'}
[23518]  {:unknown, 'k'}
[23519]  {:unknown, ','}
[23520]  {:unknown, ' '}
[23521]  {:unknown, 'a'}
[23522]  {:unknown, 'n'}
[23523]  {:unknown, 'd'}
[23524]  {:unknown, ' '}
[23525]  {:unknown, 't'}
[23526]  {:unknown, 'h'}
[23527]  {:unknown, 'e'}
[23528]  {:unknown, 'n'}
[23529]  {:unknown, ' '}
[23530]  {:unknown, 't'}
[23531]  {:unknown, 'h'}
[23532]  {:unknown, 'e'}
[23533]  {:unknown, ' '}
[23534]  {:unknown, 'o'}
[23535]  {:unknown, 'r'}
[23536]  {:unknown, 'b'}
[23537]  {:unknown, ' '}
[23538]  {:unknown, 'e'}
[23539]  {:unknown, 'v'}
[23540]  {:unknown, 'a'}
[23541]  {:unknown, 'p'}
[23542]  {:unknown, 'o'}
[23543]  {:unknown, 'r'}
[23544]  {:unknown, 'a'}
[23545]  {:unknown, 't'}
[23546]  {:unknown, 'e'}
[23547]  {:unknown, 's'}
[23548]  {:unknown, '.'}
[23549]  {:unknown, ' '}
[23550]  {:unknown, ' '}
[23551]  {:unknown, 'D'}
[23552]  {:unknown, 'i'}
[23553]  {:unknown, 'd'}
[23554]  {:unknown, ' '}
[23555]  {:unknown, 'w'}
[23556]  {:unknown, 'e'}
[23557]  {:unknown, ' '}
[23558]  {:unknown, 'd'}
[23559]  {:unknown, 'o'}
[23560]  {:unknown, ' '}
[23561]  {:unknown, 's'}
[23562]  {:unknown, 'o'}
[23563]  {:unknown, 'm'}
[23564]  {:unknown, 'e'}
[23565]  {:unknown, 't'}
[23566]  {:unknown, 'h'}
[23567]  {:unknown, 'i'}
[23568]  {:unknown, 'n'}
[23569]  {:unknown, 'g'}
[23570]  {:unknown, ' '}
[23571]  {:unknown, 'w'}
[23572]  {:unknown, 'r'}
[23573]  {:unknown, 'o'}
[23574]  {:unknown, 'n'}
[23575]  {:unknown, 'g'}
[23576]  {:unknown, '?'}
[23577]  {:unknown, ' '}
[23578]  {:unknown, ' '}
[23579]  {:unknown, 'D'}
[23580]  {:unknown, 'o'}
[23581]  {:unknown, 'e'}
[23582]  {:unknown, 's'}
[23583]  {:unknown, 'n'}
[23584]  {:unknown, '\''}
[23585]  {:unknown, 't'}
[23586]  {:unknown, ' '}
[23587]  {:unknown, 't'}
[23588]  {:unknown, 'h'}
[23589]  {:unknown, 'e'}
[23590]  {:unknown, ' '}
[23591]  {:unknown, 'd'}
[23592]  {:unknown, 'o'}
[23593]  {:unknown, 'o'}
[23594]  {:unknown, 'r'}
[23595]  {:unknown, ' '}
[23596]  {:unknown, 'l'}
[23597]  {:unknown, 'i'}
[23598]  {:unknown, 'k'}
[23599]  {:unknown, 'e'}
[23600]  {:unknown, ' '}
[23601]  {:unknown, 'u'}
[23602]  {:unknown, 's'}
[23603]  {:unknown, '?'}
[23604]  {:unknown, ' '}
[23605]  {:unknown, ' '}
[23606]  {:unknown, 'W'}
[23607]  {:unknown, 'e'}
[23608]  {:unknown, ' '}
[23609]  {:unknown, 'a'}
[23610]  {:unknown, 'l'}
[23611]  {:unknown, 's'}
[23612]  {:unknown, 'o'}
[23613]  {:unknown, ' '}
[23614]  {:unknown, 'f'}
[23615]  {:unknown, 'o'}
[23616]  {:unknown, 'u'}
[23617]  {:unknown, 'n'}
[23618]  {:unknown, 'd'}
[23619]  {:unknown, ' '}
[23620]  {:unknown, 'a'}
[23621]  {:unknown, ' '}
[23622]  {:unknown, 's'}
[23623]  {:unknown, 'm'}
[23624]  {:unknown, 'a'}
[23625]  {:unknown, 'l'}
[23626]  {:unknown, 'l'}
[23627]  {:unknown, ' '}
[23628]  {:unknown, 'h'}
[23629]  {:unknown, 'o'}
[23630]  {:unknown, 'u'}
[23631]  {:unknown, 'r'}
[23632]  {:unknown, 'g'}
[23633]  {:unknown, 'l'}
[23634]  {:unknown, 'a'}
[23635]  {:unknown, 's'}
[23636]  {:unknown, 's'}
[23637]  {:unknown, ' '}
[23638]  {:unknown, 'n'}
[23639]  {:unknown, 'e'}
[23640]  {:unknown, 'a'}
[23641]  {:unknown, 'r'}
[23642]  {:unknown, ' '}
[23643]  {:unknown, 't'}
[23644]  {:unknown, 'h'}
[23645]  {:unknown, 'e'}
[23646]  {:unknown, ' '}
[23647]  {:unknown, 'd'}
[23648]  {:unknown, 'o'}
[23649]  {:unknown, 'o'}
[23650]  {:unknown, 'r'}
[23651]  {:unknown, ','}
[23652]  {:unknown, ' '}
[23653]  {:unknown, 'e'}
[23654]  {:unknown, 'n'}
[23655]  {:unknown, 'd'}
[23656]  {:unknown, 'l'}
[23657]  {:unknown, 'e'}
[23658]  {:unknown, 's'}
[23659]  {:unknown, 's'}
[23660]  {:unknown, 'l'}
[23661]  {:unknown, 'y'}
[23662]  {:unknown, ' '}
[23663]  {:unknown, 'r'}
[23664]  {:unknown, 'u'}
[23665]  {:unknown, 'n'}
[23666]  {:unknown, 'n'}
[23667]  {:unknown, 'i'}
[23668]  {:unknown, 'n'}
[23669]  {:unknown, 'g'}
[23670]  {:unknown, '.'}
[23671]  {:unknown, ' '}
[23672]  {:unknown, ' '}
[23673]  {:unknown, 'I'}
[23674]  {:unknown, 's'}
[23675]  {:unknown, ' '}
[23676]  {:unknown, 'i'}
[23677]  {:unknown, 't'}
[23678]  {:unknown, ' '}
[23679]  {:unknown, 'w'}
[23680]  {:unknown, 'a'}
[23681]  {:unknown, 'i'}
[23682]  {:unknown, 't'}
[23683]  {:unknown, 'i'}
[23684]  {:unknown, 'n'}
[23685]  {:unknown, 'g'}
[23686]  {:unknown, ' '}
[23687]  {:unknown, 'f'}
[23688]  {:unknown, 'o'}
[23689]  {:unknown, 'r'}
[23690]  {:unknown, ' '}
[23691]  {:unknown, 's'}
[23692]  {:unknown, 'o'}
[23693]  {:unknown, 'm'}
[23694]  {:unknown, 'e'}
[23695]  {:unknown, 't'}
[23696]  {:unknown, 'h'}
[23697]  {:unknown, 'i'}
[23698]  {:unknown, 'n'}
[23699]  {:unknown, 'g'}
[23700]  {:unknown, '?'}
[23701]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[23705]  {:unknown, 'y'}
[23706]  {:unknown, ' '}
[23707]  {:unknown, '1'}
[23708]  {:unknown, '3'}
[23709]  {:unknown, ':'}
[23710]  {:unknown, ' '}
[23711]  {:unknown, 'S'}
[23712]  {:unknown, 'o'}
[23713]  {:unknown, 'm'}
[23714]  {:unknown, 'e'}
[23715]  {:unknown, ' '}
[23716]  {:unknown, 'o'}
[23717]  {:unknown, 'f'}
[23718]  {:unknown, ' '}
[23719]  {:unknown, 'm'}
[23720]  {:unknown, 'y'}
[23721]  {:unknown, ' '}
[23722]  {:unknown, 'c'}
[23723]  {:unknown, 'r'}
[23724]  {:unknown, 'e'}
[23725]  {:unknown, 'w'}
[23726]  {:unknown, ' '}
[23727]  {:unknown, 's'}
[23728]  {:unknown, 'w'}
[23729]  {:unknown, 'e'}
[23730]  {:unknown, 'a'}
[23731]  {:unknown, 'r'}
[23732]  {:unknown, ' '}
[23733]  {:unknown, 't'}
[23734]  {:unknown, 'h'}
[23735]  {:unknown, 'e'}
[23736]  {:unknown, ' '}
[23737]  {:unknown, 'o'}
[23738]  {:unknown, 'r'}
[23739]  {:unknown, 'b'}
[23740]  {:unknown, ' '}
[23741]  {:unknown, 'a'}
[23742]  {:unknown, 'c'}
[23743]  {:unknown, 't'}
[23744]  {:unknown, 'u'}
[23745]  {:unknown, 'a'}
[23746]  {:unknown, 'l'}
[23747]  {:unknown, 'l'}
[23748]  {:unknown, 'y'}
[23749]  {:unknown, ' '}
[23750]  {:unknown, 'g'}
[23751]  {:unknown, 'e'}
[23752]  {:unknown, 't'}
[23753]  {:unknown, 's'}
[23754]  {:unknown, ' '}
[23755]  {:unknown, 'h'}
[23756]  {:unknown, 'e'}
[23757]  {:unknown, 'a'}
[23758]  {:unknown, 'v'}
[23759]  {:unknown, 'e'}
[23760]  {:unknown, 'r'}
[23761]  {:unknown, ' '}
[23762]  {:unknown, 'o'}
[23763]  {:unknown, 'r'}
[23764]  {:unknown, ' '}
[23765]  {:unknown, 'l'}
[23766]  {:unknown, 'i'}
[23767]  {:unknown, 'g'}
[23768]  {:unknown, 'h'}
[23769]  {:unknown, 't'}
[23770]  {:unknown, 'e'}
[23771]  {:unknown, 'r'}
[23772]  {:unknown, ' '}
[23773]  {:unknown, 'a'}
[23774]  {:unknown, 's'}
[23775]  {:unknown, ' '}
[23776]  {:unknown, 't'}
[23777]  {:unknown, 'h'}
[23778]  {:unknown, 'e'}
[23779]  {:unknown, 'y'}
[23780]  {:unknown, ' '}
[23781]  {:unknown, 'w'}
[23782]  {:unknown, 'a'}
[23783]  {:unknown, 'l'}
[23784]  {:unknown, 'k'}
[23785]  {:unknown, ' '}
[23786]  {:unknown, 'a'}
[23787]  {:unknown, 'r'}
[23788]  {:unknown, 'o'}
[23789]  {:unknown, 'u'}
[23790]  {:unknown, 'n'}
[23791]  {:unknown, 'd'}
[23792]  {:unknown, ' '}
[23793]  {:unknown, 'w'}
[23794]  {:unknown, 'i'}
[23795]  {:unknown, 't'}
[23796]  {:unknown, 'h'}
[23797]  {:unknown, ' '}
[23798]  {:unknown, 'i'}
[23799]  {:unknown, 't'}
[23800]  {:unknown, '.'}
[23801]  {:unknown, ' '}
[23802]  {:unknown, ' '}
[23803]  {:unknown, 'I'}
[23804]  {:unknown, 's'}
[23805]  {:unknown, ' '}
[23806]  {:unknown, 't'}
[23807]  {:unknown, 'h'}
[23808]  {:unknown, 'a'}
[23809]  {:unknown, 't'}
[23810]  {:unknown, ' '}
[23811]  {:unknown, 'e'}
[23812]  {:unknown, 'v'}
[23813]  {:unknown, 'e'}
[23814]  {:unknown, 'n'}
[23815]  {:unknown, ' '}
[23816]  {:unknown, 'p'}
[23817]  {:unknown, 'o'}
[23818]  {:unknown, 's'}
[23819]  {:unknown, 's'}
[23820]  {:unknown, 'i'}
[23821]  {:unknown, 'b'}
[23822]  {:unknown, 'l'}
[23823]  {:unknown, 'e'}
[23824]  {:unknown, '?'}
[23825]  {:unknown, ' '}
[23826]  {:unknown, ' '}
[23827]  {:unknown, 'T'}
[23828]  {:unknown, 'h'}
[23829]  {:unknown, 'e'}
[23830]  {:unknown, 'y'}
[23831]  {:unknown, ' '}
[23832]  {:unknown, 's'}
[23833]  {:unknown, 'a'}
[23834]  {:unknown, 'y'}
[23835]  {:unknown, ' '}
[23836]  {:unknown, 't'}
[23837]  {:unknown, 'h'}
[23838]  {:unknown, 'a'}
[23839]  {:unknown, 't'}
[23840]  {:unknown, ' '}
[23841]  {:unknown, 'i'}
[23842]  {:unknown, 'f'}
[23843]  {:unknown, ' '}
[23844]  {:unknown, 't'}
[23845]  {:unknown, 'h'}
[23846]  {:unknown, 'e'}
[23847]  {:unknown, 'y'}
[23848]  {:unknown, ' '}
[23849]  {:unknown, 'w'}
[23850]  {:unknown, 'a'}
[23851]  {:unknown, 'l'}
[23852]  {:unknown, 'k'}
[23853]  {:unknown, ' '}
[23854]  {:unknown, 't'}
[23855]  {:unknown, 'h'}
[23856]  {:unknown, 'r'}
[23857]  {:unknown, 'o'}
[23858]  {:unknown, 'u'}
[23859]  {:unknown, 'g'}
[23860]  {:unknown, 'h'}
[23861]  {:unknown, ' '}
[23862]  {:unknown, 'c'}
[23863]  {:unknown, 'e'}
[23864]  {:unknown, 'r'}
[23865]  {:unknown, 't'}
[23866]  {:unknown, 'a'}
[23867]  {:unknown, 'i'}
[23868]  {:unknown, 'n'}
[23869]  {:unknown, ' '}
[23870]  {:unknown, 'r'}
[23871]  {:unknown, 'o'}
[23872]  {:unknown, 'o'}
[23873]  {:unknown, 'm'}
[23874]  {:unknown, 's'}
[23875]  {:unknown, ' '}
[23876]  {:unknown, 'r'}
[23877]  {:unknown, 'e'}
[23878]  {:unknown, 'p'}
[23879]  {:unknown, 'e'}
[23880]  {:unknown, 'a'}
[23881]  {:unknown, 't'}
[23882]  {:unknown, 'e'}
[23883]  {:unknown, 'd'}
[23884]  {:unknown, 'l'}
[23885]  {:unknown, 'y'}
[23886]  {:unknown, ','}
[23887]  {:unknown, ' '}
[23888]  {:unknown, 't'}
[23889]  {:unknown, 'h'}
[23890]  {:unknown, 'e'}
[23891]  {:unknown, 'y'}
[23892]  {:unknown, ' '}
[23893]  {:unknown, 'f'}
[23894]  {:unknown, 'e'}
[23895]  {:unknown, 'e'}
[23896]  {:unknown, 'l'}
[23897]  {:unknown, ' '}
[23898]  {:unknown, 'i'}
[23899]  {:unknown, 't'}
[23900]  {:unknown, ' '}
[23901]  {:unknown, 'g'}
[23902]  {:unknown, 'e'}
[23903]  {:unknown, 't'}
[23904]  {:unknown, 't'}
[23905]  {:unknown, 'i'}
[23906]  {:unknown, 'n'}
[23907]  {:unknown, 'g'}
[23908]  {:unknown, ' '}
[23909]  {:unknown, 'l'}
[23910]  {:unknown, 'i'}
[23911]  {:unknown, 'g'}
[23912]  {:unknown, 'h'}
[23913]  {:unknown, 't'}
[23914]  {:unknown, 'e'}
[23915]  {:unknown, 'r'}
[23916]  {:unknown, ' '}
[23917]  {:unknown, 'a'}
[23918]  {:unknown, 'n'}
[23919]  {:unknown, 'd'}
[23920]  {:unknown, ' '}
[23921]  {:unknown, 'l'}
[23922]  {:unknown, 'i'}
[23923]  {:unknown, 'g'}
[23924]  {:unknown, 'h'}
[23925]  {:unknown, 't'}
[23926]  {:unknown, 'e'}
[23927]  {:unknown, 'r'}
[23928]  {:unknown, ','}
[23929]  {:unknown, ' '}
[23930]  {:unknown, 'b'}
[23931]  {:unknown, 'u'}
[23932]  {:unknown, 't'}
[23933]  {:unknown, ' '}
[23934]  {:unknown, 'i'}
[23935]  {:unknown, 't'}
[23936]  {:unknown, ' '}
[23937]  {:unknown, 'e'}
[23938]  {:unknown, 'v'}
[23939]  {:unknown, 'e'}
[23940]  {:unknown, 'n'}
[23941]  {:unknown, 't'}
[23942]  {:unknown, 'u'}
[23943]  {:unknown, 'a'}
[23944]  {:unknown, 'l'}
[23945]  {:unknown, 'l'}
[23946]  {:unknown, 'y'}
[23947]  {:unknown, ' '}
[23948]  {:unknown, 'j'}
[23949]  {:unknown, 'u'}
[23950]  {:unknown, 's'}
[23951]  {:unknown, 't'}
[23952]  {:unknown, ' '}
[23953]  {:unknown, 'e'}
[23954]  {:unknown, 'v'}
[23955]  {:unknown, 'a'}
[23956]  {:unknown, 'p'}
[23957]  {:unknown, 'o'}
[23958]  {:unknown, 'r'}
[23959]  {:unknown, 'a'}
[23960]  {:unknown, 't'}
[23961]  {:unknown, 'e'}
[23962]  {:unknown, 's'}
[23963]  {:unknown, ' '}
[23964]  {:unknown, 'a'}
[23965]  {:unknown, 'n'}
[23966]  {:unknown, 'd'}
[23967]  {:unknown, ' '}
[23968]  {:unknown, 'a'}
[23969]  {:unknown, ' '}
[23970]  {:unknown, 'n'}
[23971]  {:unknown, 'e'}
[23972]  {:unknown, 'w'}
[23973]  {:unknown, ' '}
[23974]  {:unknown, 'o'}
[23975]  {:unknown, 'n'}
[23976]  {:unknown, 'e'}
[23977]  {:unknown, ' '}
[23978]  {:unknown, 'a'}
[23979]  {:unknown, 'p'}
[23980]  {:unknown, 'p'}
[23981]  {:unknown, 'e'}
[23982]  {:unknown, 'a'}
[23983]  {:unknown, 'r'}
[23984]  {:unknown, 's'}
[23985]  {:unknown, ' '}
[23986]  {:unknown, 'a'}
[23987]  {:unknown, 's'}
[23988]  {:unknown, ' '}
[23989]  {:unknown, 'u'}
[23990]  {:unknown, 's'}
[23991]  {:unknown, 'u'}
[23992]  {:unknown, 'a'}
[23993]  {:unknown, 'l'}
[23994]  {:unknown, '.'}
[23995]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[23999]  {:unknown, 'y'}
[24000]  {:unknown, ' '}
[24001]  {:unknown, '2'}
[24002]  {:unknown, '1'}
[24003]  {:unknown, ':'}
[24004]  {:unknown, ' '}
[24005]  {:unknown, 'N'}
[24006]  {:unknown, 'o'}
[24007]  {:unknown, 'w'}
[24008]  {:unknown, ' '}
[24009]  {:unknown, 'I'}
[24010]  {:unknown, ' '}
[24011]  {:unknown, 'c'}
[24012]  {:unknown, 'a'}
[24013]  {:unknown, 'n'}
[24014]  {:unknown, ' '}
[24015]  {:unknown, 'f'}
[24016]  {:unknown, 'e'}
[24017]  {:unknown, 'e'}
[24018]  {:unknown, 'l'}
[24019]  {:unknown, ' '}
[24020]  {:unknown, 't'}
[24021]  {:unknown, 'h'}
[24022]  {:unknown, 'e'}
[24023]  {:unknown, ' '}
[24024]  {:unknown, 'o'}
[24025]  {:unknown, 'r'}
[24026]  {:unknown, 'b'}
[24027]  {:unknown, ' '}
[24028]  {:unknown, 'c'}
[24029]  {:unknown, 'h'}
[24030]  {:unknown, 'a'}
[24031]  {:unknown, 'n'}
[24032]  {:unknown, 'g'}
[24033]  {:unknown, 'i'}
[24034]  {:unknown, 'n'}
[24035]  {:unknown, 'g'}
[24036]  {:unknown, ' '}
[24037]  {:unknown, 'w'}
[24038]  {:unknown, 'e'}
[24039]  {:unknown, 'i'}
[24040]  {:unknown, 'g'}
[24041]  {:unknown, 'h'}
[24042]  {:unknown, 't'}
[24043]  {:unknown, ' '}
[24044]  {:unknown, 'a'}
[24045]  {:unknown, 's'}
[24046]  {:unknown, ' '}
[24047]  {:unknown, 'I'}
[24048]  {:unknown, ' '}
[24049]  {:unknown, 'w'}
[24050]  {:unknown, 'a'}
[24051]  {:unknown, 'l'}
[24052]  {:unknown, 'k'}
[24053]  {:unknown, ' '}
[24054]  {:unknown, 'a'}
[24055]  {:unknown, 'r'}
[24056]  {:unknown, 'o'}
[24057]  {:unknown, 'u'}
[24058]  {:unknown, 'n'}
[24059]  {:unknown, 'd'}
[24060]  {:unknown, '.'}
[24061]  {:unknown, ' '}
[24062]  {:unknown, ' '}
[24063]  {:unknown, 'I'}
[24064]  {:unknown, 't'}
[24065]  {:unknown, ' '}
[24066]  {:unknown, 'd'}
[24067]  {:unknown, 'e'}
[24068]  {:unknown, 'p'}
[24069]  {:unknown, 'e'}
[24070]  {:unknown, 'n'}
[24071]  {:unknown, 'd'}
[24072]  {:unknown, 's'}
[24073]  {:unknown, ' '}
[24074]  {:unknown, 'o'}
[24075]  {:unknown, 'n'}
[24076]  {:unknown, ' '}
[24077]  {:unknown, 't'}
[24078]  {:unknown, 'h'}
[24079]  {:unknown, 'e'}
[24080]  {:unknown, ' '}
[24081]  {:unknown, 'a'}
[24082]  {:unknown, 'r'}
[24083]  {:unknown, 'e'}
[24084]  {:unknown, 'a'}
[24085]  {:unknown, ' '}
[24086]  {:unknown, '-'}
[24087]  {:unknown, ' '}
[24088]  {:unknown, 't'}
[24089]  {:unknown, 'h'}
[24090]  {:unknown, 'e'}
[24091]  {:unknown, ' '}
[24092]  {:unknown, 'c'}
[24093]  {:unknown, 'h'}
[24094]  {:unknown, 'a'}
[24095]  {:unknown, 'n'}
[24096]  {:unknown, 'g'}
[24097]  {:unknown, 'e'}
[24098]  {:unknown, ' '}
[24099]  {:unknown, 'i'}
[24100]  {:unknown, 's'}
[24101]  {:unknown, ' '}
[24102]  {:unknown, 'v'}
[24103]  {:unknown, 'e'}
[24104]  {:unknown, 'r'}
[24105]  {:unknown, 'y'}
[24106]  {:unknown, ' '}
[24107]  {:unknown, 's'}
[24108]  {:unknown, 'u'}
[24109]  {:unknown, 'b'}
[24110]  {:unknown, 't'}
[24111]  {:unknown, 'l'}
[24112]  {:unknown, 'e'}
[24113]  {:unknown, ' '}
[24114]  {:unknown, 'i'}
[24115]  {:unknown, 'n'}
[24116]  {:unknown, ' '}
[24117]  {:unknown, 's'}
[24118]  {:unknown, 'o'}
[24119]  {:unknown, 'm'}
[24120]  {:unknown, 'e'}
[24121]  {:unknown, ' '}
[24122]  {:unknown, 'p'}
[24123]  {:unknown, 'l'}
[24124]  {:unknown, 'a'}
[24125]  {:unknown, 'c'}
[24126]  {:unknown, 'e'}
[24127]  {:unknown, 's'}
[24128]  {:unknown, ','}
[24129]  {:unknown, ' '}
[24130]  {:unknown, 'b'}
[24131]  {:unknown, 'u'}
[24132]  {:unknown, 't'}
[24133]  {:unknown, ' '}
[24134]  {:unknown, 'c'}
[24135]  {:unknown, 'e'}
[24136]  {:unknown, 'r'}
[24137]  {:unknown, 't'}
[24138]  {:unknown, 'a'}
[24139]  {:unknown, 'i'}
[24140]  {:unknown, 'n'}
[24141]  {:unknown, 'l'}
[24142]  {:unknown, 'y'}
[24143]  {:unknown, ' '}
[24144]  {:unknown, 'm'}
[24145]  {:unknown, 'o'}
[24146]  {:unknown, 'r'}
[24147]  {:unknown, 'e'}
[24148]  {:unknown, ' '}
[24149]  {:unknown, 'n'}
[24150]  {:unknown, 'o'}
[24151]  {:unknown, 't'}
[24152]  {:unknown, 'i'}
[24153]  {:unknown, 'c'}
[24154]  {:unknown, 'e'}
[24155]  {:unknown, 'a'}
[24156]  {:unknown, 'b'}
[24157]  {:unknown, 'l'}
[24158]  {:unknown, 'e'}
[24159]  {:unknown, ' '}
[24160]  {:unknown, 'i'}
[24161]  {:unknown, 'n'}
[24162]  {:unknown, ' '}
[24163]  {:unknown, 'o'}
[24164]  {:unknown, 't'}
[24165]  {:unknown, 'h'}
[24166]  {:unknown, 'e'}
[24167]  {:unknown, 'r'}
[24168]  {:unknown, 's'}
[24169]  {:unknown, ','}
[24170]  {:unknown, ' '}
[24171]  {:unknown, 'e'}
[24172]  {:unknown, 's'}
[24173]  {:unknown, 'p'}
[24174]  {:unknown, 'e'}
[24175]  {:unknown, 'c'}
[24176]  {:unknown, 'i'}
[24177]  {:unknown, 'a'}
[24178]  {:unknown, 'l'}
[24179]  {:unknown, 'l'}
[24180]  {:unknown, 'y'}
[24181]  {:unknown, ' '}
[24182]  {:unknown, 'w'}
[24183]  {:unknown, 'h'}
[24184]  {:unknown, 'e'}
[24185]  {:unknown, 'n'}
[24186]  {:unknown, ' '}
[24187]  {:unknown, 'I'}
[24188]  {:unknown, ' '}
[24189]  {:unknown, 'w'}
[24190]  {:unknown, 'a'}
[24191]  {:unknown, 'l'}
[24192]  {:unknown, 'k'}
[24193]  {:unknown, ' '}
[24194]  {:unknown, 'i'}
[24195]  {:unknown, 'n'}
[24196]  {:unknown, 't'}
[24197]  {:unknown, 'o'}
[24198]  {:unknown, ' '}
[24199]  {:unknown, 'a'}
[24200]  {:unknown, ' '}
[24201]  {:unknown, 'r'}
[24202]  {:unknown, 'o'}
[24203]  {:unknown, 'o'}
[24204]  {:unknown, 'm'}
[24205]  {:unknown, ' '}
[24206]  {:unknown, 'w'}
[24207]  {:unknown, 'i'}
[24208]  {:unknown, 't'}
[24209]  {:unknown, 'h'}
[24210]  {:unknown, ' '}
[24211]  {:unknown, 'a'}
[24212]  {:unknown, ' '}
[24213]  {:unknown, 'l'}
[24214]  {:unknown, 'a'}
[24215]  {:unknown, 'r'}
[24216]  {:unknown, 'g'}
[24217]  {:unknown, 'e'}
[24218]  {:unknown, 'r'}
[24219]  {:unknown, ' '}
[24220]  {:unknown, 'n'}
[24221]  {:unknown, 'u'}
[24222]  {:unknown, 'm'}
[24223]  {:unknown, 'b'}
[24224]  {:unknown, 'e'}
[24225]  {:unknown, 'r'}
[24226]  {:unknown, ' '}
[24227]  {:unknown, 'o'}
[24228]  {:unknown, 'r'}
[24229]  {:unknown, ' '}
[24230]  {:unknown, 'o'}
[24231]  {:unknown, 'u'}
[24232]  {:unknown, 't'}
[24233]  {:unknown, ' '}
[24234]  {:unknown, 'o'}
[24235]  {:unknown, 'f'}
[24236]  {:unknown, ' '}
[24237]  {:unknown, 'a'}
[24238]  {:unknown, ' '}
[24239]  {:unknown, 'r'}
[24240]  {:unknown, 'o'}
[24241]  {:unknown, 'o'}
[24242]  {:unknown, 'm'}
[24243]  {:unknown, ' '}
[24244]  {:unknown, 'm'}
[24245]  {:unknown, 'a'}
[24246]  {:unknown, 'r'}
[24247]  {:unknown, 'k'}
[24248]  {:unknown, 'e'}
[24249]  {:unknown, 'd'}
[24250]  {:unknown, ' '}
[24251]  {:unknown, '\''}
[24252]  {:unknown, '*'}
[24253]  {:unknown, '\''}
[24254]  {:unknown, '.'}
[24255]  {:unknown, ' '}
[24256]  {:unknown, ' '}
[24257]  {:unknown, 'P'}
[24258]  {:unknown, 'e'}
[24259]  {:unknown, 'r'}
[24260]  {:unknown, 'h'}
[24261]  {:unknown, 'a'}
[24262]  {:unknown, 'p'}
[24263]  {:unknown, 's'}
[24264]  {:unknown, ' '}
[24265]  {:unknown, 'w'}
[24266]  {:unknown, 'e'}
[24267]  {:unknown, ' '}
[24268]  {:unknown, 'c'}
[24269]  {:unknown, 'a'}
[24270]  {:unknown, 'n'}
[24271]  {:unknown, ' '}
[24272]  {:unknown, 'a'}
[24273]  {:unknown, 'c'}
[24274]  {:unknown, 't'}
[24275]  {:unknown, 'u'}
[24276]  {:unknown, 'a'}
[24277]  {:unknown, 'l'}
[24278]  {:unknown, 'l'}
[24279]  {:unknown, 'y'}
[24280]  {:unknown, ' '}
[24281]  {:unknown, 'c'}
[24282]  {:unknown, 'o'}
[24283]  {:unknown, 'n'}
[24284]  {:unknown, 't'}
[24285]  {:unknown, 'r'}
[24286]  {:unknown, 'o'}
[24287]  {:unknown, 'l'}
[24288]  {:unknown, ' '}
[24289]  {:unknown, 't'}
[24290]  {:unknown, 'h'}
[24291]  {:unknown, 'e'}
[24292]  {:unknown, ' '}
[24293]  {:unknown, 'w'}
[24294]  {:unknown, 'e'}
[24295]  {:unknown, 'i'}
[24296]  {:unknown, 'g'}
[24297]  {:unknown, 'h'}
[24298]  {:unknown, 't'}
[24299]  {:unknown, ' '}
[24300]  {:unknown, 'o'}
[24301]  {:unknown, 'f'}
[24302]  {:unknown, ' '}
[24303]  {:unknown, 't'}
[24304]  {:unknown, 'h'}
[24305]  {:unknown, 'i'}
[24306]  {:unknown, 's'}
[24307]  {:unknown, ' '}
[24308]  {:unknown, 'm'}
[24309]  {:unknown, 'y'}
[24310]  {:unknown, 's'}
[24311]  {:unknown, 't'}
[24312]  {:unknown, 'e'}
[24313]  {:unknown, 'r'}
[24314]  {:unknown, 'i'}
[24315]  {:unknown, 'o'}
[24316]  {:unknown, 'u'}
[24317]  {:unknown, 's'}
[24318]  {:unknown, ' '}
[24319]  {:unknown, 'o'}
[24320]  {:unknown, 'r'}
[24321]  {:unknown, 'b'}
[24322]  {:unknown, '?'}
[24323]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[24327]  {:unknown, 'y'}
[24328]  {:unknown, ' '}
[24329]  {:unknown, '3'}
[24330]  {:unknown, '4'}
[24331]  {:unknown, ':'}
[24332]  {:unknown, ' '}
[24333]  {:unknown, 'O'}
[24334]  {:unknown, 'n'}
[24335]  {:unknown, 'e'}
[24336]  {:unknown, ' '}
[24337]  {:unknown, 'o'}
[24338]  {:unknown, 'f'}
[24339]  {:unknown, ' '}
[24340]  {:unknown, 't'}
[24341]  {:unknown, 'h'}
[24342]  {:unknown, 'e'}
[24343]  {:unknown, ' '}
[24344]  {:unknown, 'c'}
[24345]  {:unknown, 'r'}
[24346]  {:unknown, 'e'}
[24347]  {:unknown, 'w'}
[24348]  {:unknown, 'm'}
[24349]  {:unknown, 'e'}
[24350]  {:unknown, 'm'}
[24351]  {:unknown, 'b'}
[24352]  {:unknown, 'e'}
[24353]  {:unknown, 'r'}
[24354]  {:unknown, 's'}
[24355]  {:unknown, ' '}
[24356]  {:unknown, 'w'}
[24357]  {:unknown, 'a'}
[24358]  {:unknown, 's'}
[24359]  {:unknown, ' '}
[24360]  {:unknown, 'w'}
[24361]  {:unknown, 'a'}
[24362]  {:unknown, 'n'}
[24363]  {:unknown, 'd'}
[24364]  {:unknown, 'e'}
[24365]  {:unknown, 'r'}
[24366]  {:unknown, 'i'}
[24367]  {:unknown, 'n'}
[24368]  {:unknown, 'g'}
[24369]  {:unknown, ' '}
[24370]  {:unknown, 't'}
[24371]  {:unknown, 'h'}
[24372]  {:unknown, 'e'}
[24373]  {:unknown, ' '}
[24374]  {:unknown, 'r'}
[24375]  {:unknown, 'o'}
[24376]  {:unknown, 'o'}
[24377]  {:unknown, 'm'}
[24378]  {:unknown, 's'}
[24379]  {:unknown, ' '}
[24380]  {:unknown, 't'}
[24381]  {:unknown, 'o'}
[24382]  {:unknown, 'd'}
[24383]  {:unknown, 'a'}
[24384]  {:unknown, 'y'}
[24385]  {:unknown, ' '}
[24386]  {:unknown, 'a'}
[24387]  {:unknown, 'n'}
[24388]  {:unknown, 'd'}
[24389]  {:unknown, ' '}
[24390]  {:unknown, 'c'}
[24391]  {:unknown, 'l'}
[24392]  {:unknown, 'a'}
[24393]  {:unknown, 'i'}
[24394]  {:unknown, 'm'}
[24395]  {:unknown, 'e'}
[24396]  {:unknown, 'd'}
[24397]  {:unknown, ' '}
[24398]  {:unknown, 't'}
[24399]  {:unknown, 'h'}
[24400]  {:unknown, 'a'}
[24401]  {:unknown, 't'}
[24402]  {:unknown, ' '}
[24403]  {:unknown, 't'}
[24404]  {:unknown, 'h'}
[24405]  {:unknown, 'e'}
[24406]  {:unknown, ' '}
[24407]  {:unknown, 'n'}
[24408]  {:unknown, 'u'}
[24409]  {:unknown, 'm'}
[24410]  {:unknown, 'b'}
[24411]  {:unknown, 'e'}
[24412]  {:unknown, 'r'}
[24413]  {:unknown, 's'}
[24414]  {:unknown, ' '}
[24415]  {:unknown, 'o'}
[24416]  {:unknown, 'n'}
[24417]  {:unknown, ' '}
[24418]  {:unknown, 't'}
[24419]  {:unknown, 'h'}
[24420]  {:unknown, 'e'}
[24421]  {:unknown, ' '}
[24422]  {:unknown, 'd'}
[24423]  {:unknown, 'o'}
[24424]  {:unknown, 'o'}
[24425]  {:unknown, 'r'}
[24426]  {:unknown, ' '}
[24427]  {:unknown, 'f'}
[24428]  {:unknown, 'l'}
[24429]  {:unknown, 'a'}
[24430]  {:unknown, 's'}
[24431]  {:unknown, 'h'}
[24432]  {:unknown, 'e'}
[24433]  {:unknown, 'd'}
[24434]  {:unknown, ' '}
[24435]  {:unknown, 'w'}
[24436]  {:unknown, 'h'}
[24437]  {:unknown, 'i'}
[24438]  {:unknown, 't'}
[24439]  {:unknown, 'e'}
[24440]  {:unknown, ' '}
[24441]  {:unknown, 'a'}
[24442]  {:unknown, 's'}
[24443]  {:unknown, ' '}
[24444]  {:unknown, 'h'}
[24445]  {:unknown, 'e'}
[24446]  {:unknown, ' '}
[24447]  {:unknown, 'a'}
[24448]  {:unknown, 'p'}
[24449]  {:unknown, 'p'}
[24450]  {:unknown, 'r'}
[24451]  {:unknown, 'o'}
[24452]  {:unknown, 'a'}
[24453]  {:unknown, 'c'}
[24454]  {:unknown, 'h'}
[24455]  {:unknown, 'e'}
[24456]  {:unknown, 'd'}
[24457]  {:unknown, '!'}
[24458]  {:unknown, ' '}
[24459]  {:unknown, ' '}
[24460]  {:unknown, 'H'}
[24461]  {:unknown, 'e'}
[24462]  {:unknown, ' '}
[24463]  {:unknown, 's'}
[24464]  {:unknown, 'a'}
[24465]  {:unknown, 'i'}
[24466]  {:unknown, 'd'}
[24467]  {:unknown, ' '}
[24468]  {:unknown, 't'}
[24469]  {:unknown, 'h'}
[24470]  {:unknown, 'e'}
[24471]  {:unknown, ' '}
[24472]  {:unknown, 'd'}
[24473]  {:unknown, 'o'}
[24474]  {:unknown, 'o'}
[24475]  {:unknown, 'r'}
[24476]  {:unknown, ' '}
[24477]  {:unknown, 's'}
[24478]  {:unknown, 't'}
[24479]  {:unknown, 'i'}
[24480]  {:unknown, 'l'}
[24481]  {:unknown, 'l'}
[24482]  {:unknown, ' '}
[24483]  {:unknown, 'd'}
[24484]  {:unknown, 'i'}
[24485]  {:unknown, 'd'}
[24486]  {:unknown, 'n'}
[24487]  {:unknown, '\''}
[24488]  {:unknown, 't'}
[24489]  {:unknown, ' '}
[24490]  {:unknown, 'o'}
[24491]  {:unknown, 'p'}
[24492]  {:unknown, 'e'}
[24493]  {:unknown, 'n'}
[24494]  {:unknown, ','}
[24495]  {:unknown, ' '}
[24496]  {:unknown, 'b'}
[24497]  {:unknown, 'u'}
[24498]  {:unknown, 't'}
[24499]  {:unknown, ' '}
[24500]  {:unknown, 'h'}
[24501]  {:unknown, 'e'}
[24502]  {:unknown, ' '}
[24503]  {:unknown, 'n'}
[24504]  {:unknown, 'o'}
[24505]  {:unknown, 't'}
[24506]  {:unknown, 'i'}
[24507]  {:unknown, 'c'}
[24508]  {:unknown, 'e'}
[24509]  {:unknown, 'd'}
[24510]  {:unknown, ' '}
[24511]  {:unknown, 't'}
[24512]  {:unknown, 'h'}
[24513]  {:unknown, 'a'}
[24514]  {:unknown, 't'}
[24515]  {:unknown, ' '}
[24516]  {:unknown, 't'}
[24517]  {:unknown, 'h'}
[24518]  {:unknown, 'e'}
[24519]  {:unknown, ' '}
[24520]  {:unknown, 'h'}
[24521]  {:unknown, 'o'}
[24522]  {:unknown, 'u'}
[24523]  {:unknown, 'r'}
[24524]  {:unknown, 'g'}
[24525]  {:unknown, 'l'}
[24526]  {:unknown, 'a'}
[24527]  {:unknown, 's'}
[24528]  {:unknown, 's'}
[24529]  {:unknown, ' '}
[24530]  {:unknown, 'h'}
[24531]  {:unknown, 'a'}
[24532]  {:unknown, 'd'}
[24533]  {:unknown, ' '}
[24534]  {:unknown, 'r'}
[24535]  {:unknown, 'u'}
[24536]  {:unknown, 'n'}
[24537]  {:unknown, ' '}
[24538]  {:unknown, 'o'}
[24539]  {:unknown, 'u'}
[24540]  {:unknown, 't'}
[24541]  {:unknown, ' '}
[24542]  {:unknown, 'a'}
[24543]  {:unknown, 'n'}
[24544]  {:unknown, 'd'}
[24545]  {:unknown, ' '}
[24546]  {:unknown, 'f'}
[24547]  {:unknown, 'l'}
[24548]  {:unknown, 'a'}
[24549]  {:unknown, 's'}
[24550]  {:unknown, 'h'}
[24551]  {:unknown, 'e'}
[24552]  {:unknown, 'd'}
[24553]  {:unknown, ' '}
[24554]  {:unknown, 'b'}
[24555]  {:unknown, 'l'}
[24556]  {:unknown, 'a'}
[24557]  {:unknown, 'c'}
[24558]  {:unknown, 'k'}
[24559]  {:unknown, '.'}
[24560]  {:unknown, ' '}
[24561]  {:unknown, ' '}
[24562]  {:unknown, 'W'}
[24563]  {:unknown, 'h'}
[24564]  {:unknown, 'e'}
[24565]  {:unknown, 'n'}
[24566]  {:unknown, ' '}
[24567]  {:unknown, 'w'}
[24568]  {:unknown, 'e'}
[24569]  {:unknown, ' '}
[24570]  {:unknown, 'w'}
[24571]  {:unknown, 'e'}
[24572]  {:unknown, 'n'}
[24573]  {:unknown, 't'}
[24574]  {:unknown, ' '}
[24575]  {:unknown, 't'}
[24576]  {:unknown, 'o'}
[24577]  {:unknown, ' '}
[24578]  {:unknown, 'c'}
[24579]  {:unknown, 'h'}
[24580]  {:unknown, 'e'}
[24581]  {:unknown, 'c'}
[24582]  {:unknown, 'k'}
[24583]  {:unknown, ' '}
[24584]  {:unknown, 'o'}
[24585]  {:unknown, 'n'}
[24586]  {:unknown, ' '}
[24587]  {:unknown, 'i'}
[24588]  {:unknown, 't'}
[24589]  {:unknown, ','}
[24590]  {:unknown, ' '}
[24591]  {:unknown, 'i'}
[24592]  {:unknown, 't'}
[24593]  {:unknown, ' '}
[24594]  {:unknown, 'w'}
[24595]  {:unknown, 'a'}
[24596]  {:unknown, 's'}
[24597]  {:unknown, ' '}
[24598]  {:unknown, 's'}
[24599]  {:unknown, 't'}
[24600]  {:unknown, 'i'}
[24601]  {:unknown, 'l'}
[24602]  {:unknown, 'l'}
[24603]  {:unknown, ' '}
[24604]  {:unknown, 'r'}
[24605]  {:unknown, 'u'}
[24606]  {:unknown, 'n'}
[24607]  {:unknown, 'n'}
[24608]  {:unknown, 'i'}
[24609]  {:unknown, 'n'}
[24610]  {:unknown, 'g'}
[24611]  {:unknown, ' '}
[24612]  {:unknown, 'l'}
[24613]  {:unknown, 'i'}
[24614]  {:unknown, 'k'}
[24615]  {:unknown, 'e'}
[24616]  {:unknown, ' '}
[24617]  {:unknown, 'i'}
[24618]  {:unknown, 't'}
[24619]  {:unknown, ' '}
[24620]  {:unknown, 'a'}
[24621]  {:unknown, 'l'}
[24622]  {:unknown, 'w'}
[24623]  {:unknown, 'a'}
[24624]  {:unknown, 'y'}
[24625]  {:unknown, 's'}
[24626]  {:unknown, ' '}
[24627]  {:unknown, 'd'}
[24628]  {:unknown, 'o'}
[24629]  {:unknown, 'e'}
[24630]  {:unknown, 's'}
[24631]  {:unknown, '.'}
[24632]  {:unknown, ' '}
[24633]  {:unknown, ' '}
[24634]  {:unknown, 'P'}
[24635]  {:unknown, 'e'}
[24636]  {:unknown, 'r'}
[24637]  {:unknown, 'h'}
[24638]  {:unknown, 'a'}
[24639]  {:unknown, 'p'}
[24640]  {:unknown, 's'}
[24641]  {:unknown, ' '}
[24642]  {:unknown, 'h'}
[24643]  {:unknown, 'e'}
[24644]  {:unknown, ' '}
[24645]  {:unknown, 'i'}
[24646]  {:unknown, 's'}
[24647]  {:unknown, ' '}
[24648]  {:unknown, 'g'}
[24649]  {:unknown, 'o'}
[24650]  {:unknown, 'i'}
[24651]  {:unknown, 'n'}
[24652]  {:unknown, 'g'}
[24653]  {:unknown, ' '}
[24654]  {:unknown, 'm'}
[24655]  {:unknown, 'a'}
[24656]  {:unknown, 'd'}
[24657]  {:unknown, '?'}
[24658]  {:unknown, ' '}
[24659]  {:unknown, ' '}
[24660]  {:unknown, 'I'}
[24661]  {:unknown, 'f'}
[24662]  {:unknown, ' '}
[24663]  {:unknown, 'n'}
[24664]  {:unknown, 'o'}
[24665]  {:unknown, 't'}
[24666]  {:unknown, ','}
[24667]  {:unknown, ' '}
[24668]  {:unknown, 'w'}
[24669]  {:unknown, 'h'}
[24670]  {:unknown, 'i'}
[24671]  {:unknown, 'c'}
[24672]  {:unknown, 'h'}
[24673]  {:unknown, ' '}
[24674]  {:unknown, 'd'}
[24675]  {:unknown, 'o'}
[24676]  {:unknown, ' '}
[24677]  {:unknown, 'w'}
[24678]  {:unknown, 'e'}
[24679]  {:unknown, ' '}
[24680]  {:unknown, 'n'}
[24681]  {:unknown, 'e'}
[24682]  {:unknown, 'e'}
[24683]  {:unknown, 'd'}
[24684]  {:unknown, ' '}
[24685]  {:unknown, 't'}
[24686]  {:unknown, 'o'}
[24687]  {:unknown, ' '}
[24688]  {:unknown, 'a'}
[24689]  {:unknown, 'p'}
[24690]  {:unknown, 'p'}
[24691]  {:unknown, 'e'}
[24692]  {:unknown, 'a'}
[24693]  {:unknown, 's'}
[24694]  {:unknown, 'e'}
[24695]  {:unknown, ':'}
[24696]  {:unknown, ' '}
[24697]  {:unknown, 't'}
[24698]  {:unknown, 'h'}
[24699]  {:unknown, 'e'}
[24700]  {:unknown, ' '}
[24701]  {:unknown, 'd'}
[24702]  {:unknown, 'o'}
[24703]  {:unknown, 'o'}
[24704]  {:unknown, 'r'}
[24705]  {:unknown, ' '}
[24706]  {:unknown, 'o'}
[24707]  {:unknown, 'r'}
[24708]  {:unknown, ' '}
[24709]  {:unknown, 't'}
[24710]  {:unknown, 'h'}
[24711]  {:unknown, 'e'}
[24712]  {:unknown, ' '}
[24713]  {:unknown, 'h'}
[24714]  {:unknown, 'o'}
[24715]  {:unknown, 'u'}
[24716]  {:unknown, 'r'}
[24717]  {:unknown, 'g'}
[24718]  {:unknown, 'l'}
[24719]  {:unknown, 'a'}
[24720]  {:unknown, 's'}
[24721]  {:unknown, 's'}
[24722]  {:unknown, '?'}
[24723]  {:unknown, ' '}
[24724]  {:unknown, ' '}
[24725]  {:unknown, 'B'}
[24726]  {:unknown, 'o'}
[24727]  {:unknown, 't'}
[24728]  {:unknown, 'h'}
[24729]  {:unknown, '?'}
[24730]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[24734]  {:unknown, 'y'}
[24735]  {:unknown, ' '}
[24736]  {:unknown, '5'}
[24737]  {:unknown, '5'}
[24738]  {:unknown, ':'}
[24739]  {:unknown, ' '}
[24740]  {:unknown, 'T'}
[24741]  {:unknown, 'h'}
[24742]  {:unknown, 'e'}
[24743]  {:unknown, ' '}
[24744]  {:unknown, 'f'}
[24745]  {:unknown, 'i'}
[24746]  {:unknown, 'r'}
[24747]  {:unknown, 'e'}
[24748]  {:unknown, 'f'}
[24749]  {:unknown, 'l'}
[24750]  {:unknown, 'i'}
[24751]  {:unknown, 'e'}
[24752]  {:unknown, 's'}
[24753]  {:unknown, ' '}
[24754]  {:unknown, 'a'}
[24755]  {:unknown, 'r'}
[24756]  {:unknown, 'e'}
[24757]  {:unknown, ' '}
[24758]  {:unknown, 'g'}
[24759]  {:unknown, 'e'}
[24760]  {:unknown, 't'}
[24761]  {:unknown, 't'}
[24762]  {:unknown, 'i'}
[24763]  {:unknown, 'n'}
[24764]  {:unknown, 'g'}
[24765]  {:unknown, ' '}
[24766]  {:unknown, 's'}
[24767]  {:unknown, 'u'}
[24768]  {:unknown, 's'}
[24769]  {:unknown, 'p'}
[24770]  {:unknown, 'i'}
[24771]  {:unknown, 'c'}
[24772]  {:unknown, 'i'}
[24773]  {:unknown, 'o'}
[24774]  {:unknown, 'u'}
[24775]  {:unknown, 's'}
[24776]  {:unknown, '.'}
[24777]  {:unknown, ' '}
[24778]  {:unknown, ' '}
[24779]  {:unknown, 'O'}
[24780]  {:unknown, 'n'}
[24781]  {:unknown, 'e'}
[24782]  {:unknown, ' '}
[24783]  {:unknown, 'o'}
[24784]  {:unknown, 'f'}
[24785]  {:unknown, ' '}
[24786]  {:unknown, 't'}
[24787]  {:unknown, 'h'}
[24788]  {:unknown, 'e'}
[24789]  {:unknown, 'm'}
[24790]  {:unknown, ' '}
[24791]  {:unknown, 'l'}
[24792]  {:unknown, 'o'}
[24793]  {:unknown, 'o'}
[24794]  {:unknown, 'k'}
[24795]  {:unknown, 'e'}
[24796]  {:unknown, 'd'}
[24797]  {:unknown, ' '}
[24798]  {:unknown, 'a'}
[24799]  {:unknown, 't'}
[24800]  {:unknown, ' '}
[24801]  {:unknown, 'm'}
[24802]  {:unknown, 'e'}
[24803]  {:unknown, ' '}
[24804]  {:unknown, 'f'}
[24805]  {:unknown, 'u'}
[24806]  {:unknown, 'n'}
[24807]  {:unknown, 'n'}
[24808]  {:unknown, 'y'}
[24809]  {:unknown, ' '}
[24810]  {:unknown, 't'}
[24811]  {:unknown, 'o'}
[24812]  {:unknown, 'd'}
[24813]  {:unknown, 'a'}
[24814]  {:unknown, 'y'}
[24815]  {:unknown, ' '}
[24816]  {:unknown, 'a'}
[24817]  {:unknown, 'n'}
[24818]  {:unknown, 'd'}
[24819]  {:unknown, ' '}
[24820]  {:unknown, 't'}
[24821]  {:unknown, 'h'}
[24822]  {:unknown, 'e'}
[24823]  {:unknown, 'n'}
[24824]  {:unknown, ' '}
[24825]  {:unknown, 'f'}
[24826]  {:unknown, 'l'}
[24827]  {:unknown, 'e'}
[24828]  {:unknown, 'w'}
[24829]  {:unknown, ' '}
[24830]  {:unknown, 'o'}
[24831]  {:unknown, 'f'}
[24832]  {:unknown, 'f'}
[24833]  {:unknown, '.'}
[24834]  {:unknown, ' '}
[24835]  {:unknown, ' '}
[24836]  {:unknown, 'I'}
[24837]  {:unknown, ' '}
[24838]  {:unknown, 't'}
[24839]  {:unknown, 'h'}
[24840]  {:unknown, 'i'}
[24841]  {:unknown, 'n'}
[24842]  {:unknown, 'k'}
[24843]  {:unknown, ' '}
[24844]  {:unknown, 'I'}
[24845]  {:unknown, ' '}
[24846]  {:unknown, 's'}
[24847]  {:unknown, 'a'}
[24848]  {:unknown, 'w'}
[24849]  {:unknown, ' '}
[24850]  {:unknown, 'a'}
[24851]  {:unknown, 'n'}
[24852]  {:unknown, 'o'}
[24853]  {:unknown, 't'}
[24854]  {:unknown, 'h'}
[24855]  {:unknown, 'e'}
[24856]  {:unknown, 'r'}
[24857]  {:unknown, ' '}
[24858]  {:unknown, 'o'}
[24859]  {:unknown, 'n'}
[24860]  {:unknown, 'e'}
[24861]  {:unknown, ' '}
[24862]  {:unknown, 'b'}
[24863]  {:unknown, 'l'}
[24864]  {:unknown, 'i'}
[24865]  {:unknown, 'n'}
[24866]  {:unknown, 'k'}
[24867]  {:unknown, 'i'}
[24868]  {:unknown, 'n'}
[24869]  {:unknown, 'g'}
[24870]  {:unknown, ' '}
[24871]  {:unknown, 'a'}
[24872]  {:unknown, ' '}
[24873]  {:unknown, 'l'}
[24874]  {:unknown, 'i'}
[24875]  {:unknown, 't'}
[24876]  {:unknown, 't'}
[24877]  {:unknown, 'l'}
[24878]  {:unknown, 'e'}
[24879]  {:unknown, ' '}
[24880]  {:unknown, 'f'}
[24881]  {:unknown, 'a'}
[24882]  {:unknown, 's'}
[24883]  {:unknown, 't'}
[24884]  {:unknown, 'e'}
[24885]  {:unknown, 'r'}
[24886]  {:unknown, ' '}
[24887]  {:unknown, 't'}
[24888]  {:unknown, 'h'}
[24889]  {:unknown, 'a'}
[24890]  {:unknown, 'n'}
[24891]  {:unknown, ' '}
[24892]  {:unknown, 'u'}
[24893]  {:unknown, 's'}
[24894]  {:unknown, 'u'}
[24895]  {:unknown, 'a'}
[24896]  {:unknown, 'l'}
[24897]  {:unknown, '.'}
[24898]  {:unknown, ' '}
[24899]  {:unknown, ' '}
[24900]  {:unknown, 'O'}
[24901]  {:unknown, 'r'}
[24902]  {:unknown, ' '}
[24903]  {:unknown, 'w'}
[24904]  {:unknown, 'a'}
[24905]  {:unknown, 's'}
[24906]  {:unknown, ' '}
[24907]  {:unknown, 'i'}
[24908]  {:unknown, 't'}
[24909]  {:unknown, ' '}
[24910]  {:unknown, 'a'}
[24911]  {:unknown, ' '}
[24912]  {:unknown, 'l'}
[24913]  {:unknown, 'i'}
[24914]  {:unknown, 't'}
[24915]  {:unknown, 't'}
[24916]  {:unknown, 'l'}
[24917]  {:unknown, 'e'}
[24918]  {:unknown, ' '}
[24919]  {:unknown, 's'}
[24920]  {:unknown, 'l'}
[24921]  {:unknown, 'o'}
[24922]  {:unknown, 'w'}
[24923]  {:unknown, 'e'}
[24924]  {:unknown, 'r'}
[24925]  {:unknown, '?'}
[24926]  {:unknown, ' '}
[24927]  {:unknown, ' '}
[24928]  {:unknown, 'W'}
[24929]  {:unknown, 'e'}
[24930]  {:unknown, ' '}
[24931]  {:unknown, 'a'}
[24932]  {:unknown, 'r'}
[24933]  {:unknown, 'e'}
[24934]  {:unknown, ' '}
[24935]  {:unknown, 'g'}
[24936]  {:unknown, 'e'}
[24937]  {:unknown, 't'}
[24938]  {:unknown, 't'}
[24939]  {:unknown, 'i'}
[24940]  {:unknown, 'n'}
[24941]  {:unknown, 'g'}
[24942]  {:unknown, ' '}
[24943]  {:unknown, 'b'}
[24944]  {:unknown, 'e'}
[24945]  {:unknown, 't'}
[24946]  {:unknown, 't'}
[24947]  {:unknown, 'e'}
[24948]  {:unknown, 'r'}
[24949]  {:unknown, ' '}
[24950]  {:unknown, 'a'}
[24951]  {:unknown, 't'}
[24952]  {:unknown, ' '}
[24953]  {:unknown, 'c'}
[24954]  {:unknown, 'o'}
[24955]  {:unknown, 'n'}
[24956]  {:unknown, 't'}
[24957]  {:unknown, 'r'}
[24958]  {:unknown, 'o'}
[24959]  {:unknown, 'l'}
[24960]  {:unknown, 'l'}
[24961]  {:unknown, 'i'}
[24962]  {:unknown, 'n'}
[24963]  {:unknown, 'g'}
[24964]  {:unknown, ' '}
[24965]  {:unknown, 't'}
[24966]  {:unknown, 'h'}
[24967]  {:unknown, 'e'}
[24968]  {:unknown, ' '}
[24969]  {:unknown, 'w'}
[24970]  {:unknown, 'e'}
[24971]  {:unknown, 'i'}
[24972]  {:unknown, 'g'}
[24973]  {:unknown, 'h'}
[24974]  {:unknown, 't'}
[24975]  {:unknown, ' '}
[24976]  {:unknown, 'o'}
[24977]  {:unknown, 'f'}
[24978]  {:unknown, ' '}
[24979]  {:unknown, 't'}
[24980]  {:unknown, 'h'}
[24981]  {:unknown, 'e'}
[24982]  {:unknown, ' '}
[24983]  {:unknown, 'o'}
[24984]  {:unknown, 'r'}
[24985]  {:unknown, 'b'}
[24986]  {:unknown, ','}
[24987]  {:unknown, ' '}
[24988]  {:unknown, 'a'}
[24989]  {:unknown, 'n'}
[24990]  {:unknown, 'd'}
[24991]  {:unknown, ' '}
[24992]  {:unknown, 'w'}
[24993]  {:unknown, 'e'}
[24994]  {:unknown, ' '}
[24995]  {:unknown, 't'}
[24996]  {:unknown, 'h'}
[24997]  {:unknown, 'i'}
[24998]  {:unknown, 'n'}
[24999]  {:unknown, 'k'}
[25000]  {:unknown, ' '}
[25001]  {:unknown, 't'}
[25002]  {:unknown, 'h'}
[25003]  {:unknown, 'a'}
[25004]  {:unknown, 't'}
[25005]  {:unknown, '\''}
[25006]  {:unknown, 's'}
[25007]  {:unknown, ' '}
[25008]  {:unknown, 'w'}
[25009]  {:unknown, 'h'}
[25010]  {:unknown, 'a'}
[25011]  {:unknown, 't'}
[25012]  {:unknown, ' '}
[25013]  {:unknown, 't'}
[25014]  {:unknown, 'h'}
[25015]  {:unknown, 'e'}
[25016]  {:unknown, ' '}
[25017]  {:unknown, 'n'}
[25018]  {:unknown, 'u'}
[25019]  {:unknown, 'm'}
[25020]  {:unknown, 'b'}
[25021]  {:unknown, 'e'}
[25022]  {:unknown, 'r'}
[25023]  {:unknown, 's'}
[25024]  {:unknown, ' '}
[25025]  {:unknown, 'a'}
[25026]  {:unknown, 'r'}
[25027]  {:unknown, 'e'}
[25028]  {:unknown, ' '}
[25029]  {:unknown, 'a'}
[25030]  {:unknown, 'l'}
[25031]  {:unknown, 'l'}
[25032]  {:unknown, ' '}
[25033]  {:unknown, 'a'}
[25034]  {:unknown, 'b'}
[25035]  {:unknown, 'o'}
[25036]  {:unknown, 'u'}
[25037]  {:unknown, 't'}
[25038]  {:unknown, '.'}
[25039]  {:unknown, ' '}
[25040]  {:unknown, ' '}
[25041]  {:unknown, 'T'}
[25042]  {:unknown, 'h'}
[25043]  {:unknown, 'e'}
[25044]  {:unknown, ' '}
[25045]  {:unknown, 'o'}
[25046]  {:unknown, 'r'}
[25047]  {:unknown, 'b'}
[25048]  {:unknown, ' '}
[25049]  {:unknown, 's'}
[25050]  {:unknown, 't'}
[25051]  {:unknown, 'a'}
[25052]  {:unknown, 'r'}
[25053]  {:unknown, 't'}
[25054]  {:unknown, 's'}
[25055]  {:unknown, ' '}
[25056]  {:unknown, 'a'}
[25057]  {:unknown, 't'}
[25058]  {:unknown, ' '}
[25059]  {:unknown, 't'}
[25060]  {:unknown, 'h'}
[25061]  {:unknown, 'e'}
[25062]  {:unknown, ' '}
[25063]  {:unknown, 'w'}
[25064]  {:unknown, 'e'}
[25065]  {:unknown, 'i'}
[25066]  {:unknown, 'g'}
[25067]  {:unknown, 'h'}
[25068]  {:unknown, 't'}
[25069]  {:unknown, ' '}
[25070]  {:unknown, 'l'}
[25071]  {:unknown, 'a'}
[25072]  {:unknown, 'b'}
[25073]  {:unknown, 'e'}
[25074]  {:unknown, 'l'}
[25075]  {:unknown, 'e'}
[25076]  {:unknown, 'd'}
[25077]  {:unknown, ' '}
[25078]  {:unknown, 'o'}
[25079]  {:unknown, 'n'}
[25080]  {:unknown, ' '}
[25081]  {:unknown, 't'}
[25082]  {:unknown, 'h'}
[25083]  {:unknown, 'e'}
[25084]  {:unknown, ' '}
[25085]  {:unknown, 'p'}
[25086]  {:unknown, 'e'}
[25087]  {:unknown, 'd'}
[25088]  {:unknown, 'e'}
[25089]  {:unknown, 's'}
[25090]  {:unknown, 't'}
[25091]  {:unknown, 'a'}
[25092]  {:unknown, 'l'}
[25093]  {:unknown, ','}
[25094]  {:unknown, ' '}
[25095]  {:unknown, 'a'}
[25096]  {:unknown, 'n'}
[25097]  {:unknown, 'd'}
[25098]  {:unknown, ' '}
[25099]  {:unknown, 'g'}
[25100]  {:unknown, 'o'}
[25101]  {:unknown, 'e'}
[25102]  {:unknown, 's'}
[25103]  {:unknown, ' '}
[25104]  {:unknown, 'd'}
[25105]  {:unknown, 'o'}
[25106]  {:unknown, 'w'}
[25107]  {:unknown, 'n'}
[25108]  {:unknown, ' '}
[25109]  {:unknown, 'a'}
[25110]  {:unknown, 's'}
[25111]  {:unknown, ' '}
[25112]  {:unknown, 'w'}
[25113]  {:unknown, 'e'}
[25114]  {:unknown, ' '}
[25115]  {:unknown, 'l'}
[25116]  {:unknown, 'e'}
[25117]  {:unknown, 'a'}
[25118]  {:unknown, 'v'}
[25119]  {:unknown, 'e'}
[25120]  {:unknown, ' '}
[25121]  {:unknown, 'a'}
[25122]  {:unknown, ' '}
[25123]  {:unknown, 'r'}
[25124]  {:unknown, 'o'}
[25125]  {:unknown, 'o'}
[25126]  {:unknown, 'm'}
[25127]  {:unknown, ' '}
[25128]  {:unknown, 'm'}
[25129]  {:unknown, 'a'}
[25130]  {:unknown, 'r'}
[25131]  {:unknown, 'k'}
[25132]  {:unknown, 'e'}
[25133]  {:unknown, 'd'}
[25134]  {:unknown, ' '}
[25135]  {:unknown, '\''}
[25136]  {:unknown, '-'}
[25137]  {:unknown, '\''}
[25138]  {:unknown, ','}
[25139]  {:unknown, ' '}
[25140]  {:unknown, 'u'}
[25141]  {:unknown, 'p'}
[25142]  {:unknown, ' '}
[25143]  {:unknown, 'a'}
[25144]  {:unknown, 's'}
[25145]  {:unknown, ' '}
[25146]  {:unknown, 'w'}
[25147]  {:unknown, 'e'}
[25148]  {:unknown, ' '}
[25149]  {:unknown, 'l'}
[25150]  {:unknown, 'e'}
[25151]  {:unknown, 'a'}
[25152]  {:unknown, 'v'}
[25153]  {:unknown, 'e'}
[25154]  {:unknown, ' '}
[25155]  {:unknown, 'a'}
[25156]  {:unknown, ' '}
[25157]  {:unknown, 'r'}
[25158]  {:unknown, 'o'}
[25159]  {:unknown, 'o'}
[25160]  {:unknown, 'm'}
[25161]  {:unknown, ' '}
[25162]  {:unknown, 'm'}
[25163]  {:unknown, 'a'}
[25164]  {:unknown, 'r'}
[25165]  {:unknown, 'k'}
[25166]  {:unknown, 'e'}
[25167]  {:unknown, 'd'}
[25168]  {:unknown, ' '}
[25169]  {:unknown, '\''}
[25170]  {:unknown, '+'}
[25171]  {:unknown, '\''}
[25172]  {:unknown, ','}
[25173]  {:unknown, ' '}
[25174]  {:unknown, 'a'}
[25175]  {:unknown, 'n'}
[25176]  {:unknown, 'd'}
[25177]  {:unknown, ' '}
[25178]  {:unknown, 'u'}
[25179]  {:unknown, 'p'}
[25180]  {:unknown, ' '}
[25181]  {:unknown, 'e'}
[25182]  {:unknown, 'v'}
[25183]  {:unknown, 'e'}
[25184]  {:unknown, 'n'}
[25185]  {:unknown, ' '}
[25186]  {:unknown, 'm'}
[25187]  {:unknown, 'o'}
[25188]  {:unknown, 'r'}
[25189]  {:unknown, 'e'}
[25190]  {:unknown, ' '}
[25191]  {:unknown, 'a'}
[25192]  {:unknown, 's'}
[25193]  {:unknown, ' '}
[25194]  {:unknown, 'w'}
[25195]  {:unknown, 'e'}
[25196]  {:unknown, ' '}
[25197]  {:unknown, 'l'}
[25198]  {:unknown, 'e'}
[25199]  {:unknown, 'a'}
[25200]  {:unknown, 'v'}
[25201]  {:unknown, 'e'}
[25202]  {:unknown, ' '}
[25203]  {:unknown, 'a'}
[25204]  {:unknown, ' '}
[25205]  {:unknown, 'r'}
[25206]  {:unknown, 'o'}
[25207]  {:unknown, 'o'}
[25208]  {:unknown, 'm'}
[25209]  {:unknown, ' '}
[25210]  {:unknown, 'm'}
[25211]  {:unknown, 'a'}
[25212]  {:unknown, 'r'}
[25213]  {:unknown, 'k'}
[25214]  {:unknown, 'e'}
[25215]  {:unknown, 'd'}
[25216]  {:unknown, ' '}
[25217]  {:unknown, '\''}
[25218]  {:unknown, '*'}
[25219]  {:unknown, '\''}
[25220]  {:unknown, '.'}
[25221]  {:unknown, ' '}
[25222]  {:unknown, ' '}
[25223]  {:unknown, 'E'}
[25224]  {:unknown, 'n'}
[25225]  {:unknown, 't'}
[25226]  {:unknown, 'e'}
[25227]  {:unknown, 'r'}
[25228]  {:unknown, 'i'}
[25229]  {:unknown, 'n'}
[25230]  {:unknown, 'g'}
[25231]  {:unknown, ' '}
[25232]  {:unknown, 'r'}
[25233]  {:unknown, 'o'}
[25234]  {:unknown, 'o'}
[25235]  {:unknown, 'm'}
[25236]  {:unknown, 's'}
[25237]  {:unknown, ' '}
[25238]  {:unknown, 'w'}
[25239]  {:unknown, 'i'}
[25240]  {:unknown, 't'}
[25241]  {:unknown, 'h'}
[25242]  {:unknown, ' '}
[25243]  {:unknown, 'l'}
[25244]  {:unknown, 'a'}
[25245]  {:unknown, 'r'}
[25246]  {:unknown, 'g'}
[25247]  {:unknown, 'e'}
[25248]  {:unknown, 'r'}
[25249]  {:unknown, ' '}
[25250]  {:unknown, 'n'}
[25251]  {:unknown, 'u'}
[25252]  {:unknown, 'm'}
[25253]  {:unknown, 'b'}
[25254]  {:unknown, 'e'}
[25255]  {:unknown, 'r'}
[25256]  {:unknown, 's'}
[25257]  {:unknown, ' '}
[25258]  {:unknown, 'h'}
[25259]  {:unknown, 'a'}
[25260]  {:unknown, 's'}
[25261]  {:unknown, ' '}
[25262]  {:unknown, 'a'}
[25263]  {:unknown, ' '}
[25264]  {:unknown, 'g'}
[25265]  {:unknown, 'r'}
[25266]  {:unknown, 'e'}
[25267]  {:unknown, 'a'}
[25268]  {:unknown, 't'}
[25269]  {:unknown, 'e'}
[25270]  {:unknown, 'r'}
[25271]  {:unknown, ' '}
[25272]  {:unknown, 'e'}
[25273]  {:unknown, 'f'}
[25274]  {:unknown, 'f'}
[25275]  {:unknown, 'e'}
[25276]  {:unknown, 'c'}
[25277]  {:unknown, 't'}
[25278]  {:unknown, '.'}
[25279]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[25283]  {:unknown, 'y'}
[25284]  {:unknown, ' '}
[25285]  {:unknown, '8'}
[25286]  {:unknown, '9'}
[25287]  {:unknown, ':'}
[25288]  {:unknown, ' '}
[25289]  {:unknown, 'E'}
[25290]  {:unknown, 'v'}
[25291]  {:unknown, 'e'}
[25292]  {:unknown, 'r'}
[25293]  {:unknown, 'y'}
[25294]  {:unknown, ' '}
[25295]  {:unknown, 'o'}
[25296]  {:unknown, 'n'}
[25297]  {:unknown, 'c'}
[25298]  {:unknown, 'e'}
[25299]  {:unknown, ' '}
[25300]  {:unknown, 'i'}
[25301]  {:unknown, 'n'}
[25302]  {:unknown, ' '}
[25303]  {:unknown, 'a'}
[25304]  {:unknown, ' '}
[25305]  {:unknown, 'g'}
[25306]  {:unknown, 'r'}
[25307]  {:unknown, 'e'}
[25308]  {:unknown, 'a'}
[25309]  {:unknown, 't'}
[25310]  {:unknown, ' '}
[25311]  {:unknown, 'w'}
[25312]  {:unknown, 'h'}
[25313]  {:unknown, 'i'}
[25314]  {:unknown, 'l'}
[25315]  {:unknown, 'e'}
[25316]  {:unknown, ','}
[25317]  {:unknown, ' '}
[25318]  {:unknown, 'o'}
[25319]  {:unknown, 'n'}
[25320]  {:unknown, 'e'}
[25321]  {:unknown, ' '}
[25322]  {:unknown, 'o'}
[25323]  {:unknown, 'f'}
[25324]  {:unknown, ' '}
[25325]  {:unknown, 't'}
[25326]  {:unknown, 'h'}
[25327]  {:unknown, 'e'}
[25328]  {:unknown, ' '}
[25329]  {:unknown, 'c'}
[25330]  {:unknown, 'r'}
[25331]  {:unknown, 'e'}
[25332]  {:unknown, 'w'}
[25333]  {:unknown, 'm'}
[25334]  {:unknown, 'e'}
[25335]  {:unknown, 'm'}
[25336]  {:unknown, 'b'}
[25337]  {:unknown, 'e'}
[25338]  {:unknown, 'r'}
[25339]  {:unknown, 's'}
[25340]  {:unknown, ' '}
[25341]  {:unknown, 'h'}
[25342]  {:unknown, 'a'}
[25343]  {:unknown, 's'}
[25344]  {:unknown, ' '}
[25345]  {:unknown, 't'}
[25346]  {:unknown, 'h'}
[25347]  {:unknown, 'e'}
[25348]  {:unknown, ' '}
[25349]  {:unknown, 's'}
[25350]  {:unknown, 'a'}
[25351]  {:unknown, 'm'}
[25352]  {:unknown, 'e'}
[25353]  {:unknown, ' '}
[25354]  {:unknown, 's'}
[25355]  {:unknown, 't'}
[25356]  {:unknown, 'o'}
[25357]  {:unknown, 'r'}
[25358]  {:unknown, 'y'}
[25359]  {:unknown, ':'}
[25360]  {:unknown, ' '}
[25361]  {:unknown, 't'}
[25362]  {:unknown, 'h'}
[25363]  {:unknown, 'a'}
[25364]  {:unknown, 't'}
[25365]  {:unknown, ' '}
[25366]  {:unknown, 't'}
[25367]  {:unknown, 'h'}
[25368]  {:unknown, 'e'}
[25369]  {:unknown, ' '}
[25370]  {:unknown, 'd'}
[25371]  {:unknown, 'o'}
[25372]  {:unknown, 'o'}
[25373]  {:unknown, 'r'}
[25374]  {:unknown, ' '}
[25375]  {:unknown, 'f'}
[25376]  {:unknown, 'l'}
[25377]  {:unknown, 'a'}
[25378]  {:unknown, 's'}
[25379]  {:unknown, 'h'}
[25380]  {:unknown, 'e'}
[25381]  {:unknown, 's'}
[25382]  {:unknown, ' '}
[25383]  {:unknown, 'w'}
[25384]  {:unknown, 'h'}
[25385]  {:unknown, 'i'}
[25386]  {:unknown, 't'}
[25387]  {:unknown, 'e'}
[25388]  {:unknown, ','}
[25389]  {:unknown, ' '}
[25390]  {:unknown, 't'}
[25391]  {:unknown, 'h'}
[25392]  {:unknown, 'e'}
[25393]  {:unknown, ' '}
[25394]  {:unknown, 'h'}
[25395]  {:unknown, 'o'}
[25396]  {:unknown, 'u'}
[25397]  {:unknown, 'r'}
[25398]  {:unknown, 'g'}
[25399]  {:unknown, 'l'}
[25400]  {:unknown, 'a'}
[25401]  {:unknown, 's'}
[25402]  {:unknown, 's'}
[25403]  {:unknown, ' '}
[25404]  {:unknown, 'h'}
[25405]  {:unknown, 'a'}
[25406]  {:unknown, 'd'}
[25407]  {:unknown, ' '}
[25408]  {:unknown, 'a'}
[25409]  {:unknown, 'l'}
[25410]  {:unknown, 'r'}
[25411]  {:unknown, 'e'}
[25412]  {:unknown, 'a'}
[25413]  {:unknown, 'd'}
[25414]  {:unknown, 'y'}
[25415]  {:unknown, ' '}
[25416]  {:unknown, 'r'}
[25417]  {:unknown, 'u'}
[25418]  {:unknown, 'n'}
[25419]  {:unknown, ' '}
[25420]  {:unknown, 'o'}
[25421]  {:unknown, 'u'}
[25422]  {:unknown, 't'}
[25423]  {:unknown, ','}
[25424]  {:unknown, ' '}
[25425]  {:unknown, 'i'}
[25426]  {:unknown, 't'}
[25427]  {:unknown, ' '}
[25428]  {:unknown, 'f'}
[25429]  {:unknown, 'l'}
[25430]  {:unknown, 'a'}
[25431]  {:unknown, 's'}
[25432]  {:unknown, 'h'}
[25433]  {:unknown, 'e'}
[25434]  {:unknown, 's'}
[25435]  {:unknown, ' '}
[25436]  {:unknown, 'b'}
[25437]  {:unknown, 'l'}
[25438]  {:unknown, 'a'}
[25439]  {:unknown, 'c'}
[25440]  {:unknown, 'k'}
[25441]  {:unknown, ','}
[25442]  {:unknown, ' '}
[25443]  {:unknown, 'a'}
[25444]  {:unknown, 'n'}
[25445]  {:unknown, 'd'}
[25446]  {:unknown, ' '}
[25447]  {:unknown, 't'}
[25448]  {:unknown, 'h'}
[25449]  {:unknown, 'e'}
[25450]  {:unknown, ' '}
[25451]  {:unknown, 'o'}
[25452]  {:unknown, 'r'}
[25453]  {:unknown, 'b'}
[25454]  {:unknown, ' '}
[25455]  {:unknown, 'e'}
[25456]  {:unknown, 'v'}
[25457]  {:unknown, 'a'}
[25458]  {:unknown, 'p'}
[25459]  {:unknown, 'o'}
[25460]  {:unknown, 'r'}
[25461]  {:unknown, 'a'}
[25462]  {:unknown, 't'}
[25463]  {:unknown, 'e'}
[25464]  {:unknown, 's'}
[25465]  {:unknown, '.'}
[25466]  {:unknown, ' '}
[25467]  {:unknown, ' '}
[25468]  {:unknown, 'A'}
[25469]  {:unknown, 'r'}
[25470]  {:unknown, 'e'}
[25471]  {:unknown, ' '}
[25472]  {:unknown, 'w'}
[25473]  {:unknown, 'e'}
[25474]  {:unknown, ' '}
[25475]  {:unknown, 't'}
[25476]  {:unknown, 'o'}
[25477]  {:unknown, 'o'}
[25478]  {:unknown, ' '}
[25479]  {:unknown, 's'}
[25480]  {:unknown, 'l'}
[25481]  {:unknown, 'o'}
[25482]  {:unknown, 'w'}
[25483]  {:unknown, '?'}
[25484]  {:unknown, ' '}
[25485]  {:unknown, ' '}
[25486]  {:unknown, 'W'}
[25487]  {:unknown, 'e'}
[25488]  {:unknown, ' '}
[25489]  {:unknown, 'c'}
[25490]  {:unknown, 'a'}
[25491]  {:unknown, 'n'}
[25492]  {:unknown, '\''}
[25493]  {:unknown, 't'}
[25494]  {:unknown, ' '}
[25495]  {:unknown, 's'}
[25496]  {:unknown, 'e'}
[25497]  {:unknown, 'e'}
[25498]  {:unknown, 'm'}
[25499]  {:unknown, ' '}
[25500]  {:unknown, 't'}
[25501]  {:unknown, 'o'}
[25502]  {:unknown, ' '}
[25503]  {:unknown, 'f'}
[25504]  {:unknown, 'i'}
[25505]  {:unknown, 'n'}
[25506]  {:unknown, 'd'}
[25507]  {:unknown, ' '}
[25508]  {:unknown, 'a'}
[25509]  {:unknown, ' '}
[25510]  {:unknown, 'w'}
[25511]  {:unknown, 'a'}
[25512]  {:unknown, 'y'}
[25513]  {:unknown, ' '}
[25514]  {:unknown, 't'}
[25515]  {:unknown, 'o'}
[25516]  {:unknown, ' '}
[25517]  {:unknown, 'm'}
[25518]  {:unknown, 'a'}
[25519]  {:unknown, 'k'}
[25520]  {:unknown, 'e'}
[25521]  {:unknown, ' '}
[25522]  {:unknown, 't'}
[25523]  {:unknown, 'h'}
[25524]  {:unknown, 'e'}
[25525]  {:unknown, ' '}
[25526]  {:unknown, 'o'}
[25527]  {:unknown, 'r'}
[25528]  {:unknown, 'b'}
[25529]  {:unknown, '\''}
[25530]  {:unknown, 's'}
[25531]  {:unknown, ' '}
[25532]  {:unknown, 'w'}
[25533]  {:unknown, 'e'}
[25534]  {:unknown, 'i'}
[25535]  {:unknown, 'g'}
[25536]  {:unknown, 'h'}
[25537]  {:unknown, 't'}
[25538]  {:unknown, ' '}
[25539]  {:unknown, 'm'}
[25540]  {:unknown, 'a'}
[25541]  {:unknown, 't'}
[25542]  {:unknown, 'c'}
[25543]  {:unknown, 'h'}
[25544]  {:unknown, ' '}
[25545]  {:unknown, 'w'}
[25546]  {:unknown, 'h'}
[25547]  {:unknown, 'a'}
[25548]  {:unknown, 't'}
[25549]  {:unknown, ' '}
[25550]  {:unknown, 't'}
[25551]  {:unknown, 'h'}
[25552]  {:unknown, 'e'}
[25553]  {:unknown, ' '}
[25554]  {:unknown, 'd'}
[25555]  {:unknown, 'o'}
[25556]  {:unknown, 'o'}
[25557]  {:unknown, 'r'}
[25558]  {:unknown, ' '}
[25559]  {:unknown, 'w'}
[25560]  {:unknown, 'a'}
[25561]  {:unknown, 'n'}
[25562]  {:unknown, 't'}
[25563]  {:unknown, 's'}
[25564]  {:unknown, ' '}
[25565]  {:unknown, 'b'}
[25566]  {:unknown, 'e'}
[25567]  {:unknown, 'f'}
[25568]  {:unknown, 'o'}
[25569]  {:unknown, 'r'}
[25570]  {:unknown, 'e'}
[25571]  {:unknown, ' '}
[25572]  {:unknown, 't'}
[25573]  {:unknown, 'h'}
[25574]  {:unknown, 'e'}
[25575]  {:unknown, ' '}
[25576]  {:unknown, 'h'}
[25577]  {:unknown, 'o'}
[25578]  {:unknown, 'u'}
[25579]  {:unknown, 'r'}
[25580]  {:unknown, 'g'}
[25581]  {:unknown, 'l'}
[25582]  {:unknown, 'a'}
[25583]  {:unknown, 's'}
[25584]  {:unknown, 's'}
[25585]  {:unknown, ' '}
[25586]  {:unknown, 'r'}
[25587]  {:unknown, 'u'}
[25588]  {:unknown, 'n'}
[25589]  {:unknown, 's'}
[25590]  {:unknown, ' '}
[25591]  {:unknown, 'o'}
[25592]  {:unknown, 'u'}
[25593]  {:unknown, 't'}
[25594]  {:unknown, '.'}
[25595]  {:unknown, ' '}
[25596]  {:unknown, ' '}
[25597]  {:unknown, 'I'}
[25598]  {:unknown, 'f'}
[25599]  {:unknown, ' '}
[25600]  {:unknown, 'o'}
[25601]  {:unknown, 'n'}
[25602]  {:unknown, 'l'}
[25603]  {:unknown, 'y'}
[25604]  {:unknown, ' '}
[25605]  {:unknown, 'w'}
[25606]  {:unknown, 'e'}
[25607]  {:unknown, ' '}
[25608]  {:unknown, 'c'}
[25609]  {:unknown, 'o'}
[25610]  {:unknown, 'u'}
[25611]  {:unknown, 'l'}
[25612]  {:unknown, 'd'}
[25613]  {:unknown, ' '}
[25614]  {:unknown, 'f'}
[25615]  {:unknown, 'i'}
[25616]  {:unknown, 'n'}
[25617]  {:unknown, 'd'}
[25618]  {:unknown, ' '}
[25619]  {:unknown, 'a'}
[25620]  {:unknown, ' '}
[25621]  {:unknown, 's'}
[25622]  {:unknown, 'h'}
[25623]  {:unknown, 'o'}
[25624]  {:unknown, 'r'}
[25625]  {:unknown, 't'}
[25626]  {:unknown, 'e'}
[25627]  {:unknown, 'r'}
[25628]  {:unknown, ' '}
[25629]  {:unknown, 'r'}
[25630]  {:unknown, 'o'}
[25631]  {:unknown, 'u'}
[25632]  {:unknown, 't'}
[25633]  {:unknown, 'e'}
[25634]  {:unknown, ' '}
[25635]  {:unknown, 't'}
[25636]  {:unknown, 'h'}
[25637]  {:unknown, 'r'}
[25638]  {:unknown, 'o'}
[25639]  {:unknown, 'u'}
[25640]  {:unknown, 'g'}
[25641]  {:unknown, 'h'}
[25642]  {:unknown, ' '}
[25643]  {:unknown, 't'}
[25644]  {:unknown, 'h'}
[25645]  {:unknown, 'e'}
[25646]  {:unknown, ' '}
[25647]  {:unknown, 'r'}
[25648]  {:unknown, 'o'}
[25649]  {:unknown, 'o'}
[25650]  {:unknown, 'm'}
[25651]  {:unknown, 's'}
[25652]  {:unknown, '.'}
[25653]  {:unknown, '.'}
[25654]  {:unknown, '.'}
[25655]  {:mult, {:value, 10}, {:value, 68}, {:value, 97}}
[25659]  {:unknown, 'y'}
[25660]  {:unknown, ' '}
[25661]  {:unknown, '1'}
[25662]  {:unknown, '4'}
[25663]  {:unknown, '4'}
[25664]  {:unknown, ':'}
[25665]  {:unknown, ' '}
[25666]  {:unknown, 'W'}
[25667]  {:unknown, 'e'}
[25668]  {:unknown, ' '}
[25669]  {:unknown, 'a'}
[25670]  {:unknown, 'r'}
[25671]  {:unknown, 'e'}
[25672]  {:unknown, ' '}
[25673]  {:unknown, 'a'}
[25674]  {:unknown, 'b'}
[25675]  {:unknown, 'a'}
[25676]  {:unknown, 'n'}
[25677]  {:unknown, 'd'}
[25678]  {:unknown, 'o'}
[25679]  {:unknown, 'n'}
[25680]  {:unknown, 'i'}
[25681]  {:unknown, 'n'}
[25682]  {:unknown, 'g'}
[25683]  {:unknown, ' '}
[25684]  {:unknown, 't'}
[25685]  {:unknown, 'h'}
[25686]  {:unknown, 'e'}
[25687]  {:unknown, ' '}
[25688]  {:unknown, 'm'}
[25689]  {:unknown, 'i'}
[25690]  {:unknown, 's'}
[25691]  {:unknown, 's'}
[25692]  {:unknown, 'i'}
[25693]  {:unknown, 'o'}
[25694]  {:unknown, 'n'}
[25695]  {:unknown, '.'}
[25696]  {:unknown, ' '}
[25697]  {:unknown, ' '}
[25698]  {:unknown, 'N'}
[25699]  {:unknown, 'o'}
[25700]  {:unknown, 'n'}
[25701]  {:unknown, 'e'}
[25702]  {:unknown, ' '}
[25703]  {:unknown, 'o'}
[25704]  {:unknown, 'f'}
[25705]  {:unknown, ' '}
[25706]  {:unknown, 'u'}
[25707]  {:unknown, 's'}
[25708]  {:unknown, ' '}
[25709]  {:unknown, 'c'}
[25710]  {:unknown, 'a'}
[25711]  {:unknown, 'n'}
[25712]  {:unknown, ' '}
[25713]  {:unknown, 'w'}
[25714]  {:unknown, 'o'}
[25715]  {:unknown, 'r'}
[25716]  {:unknown, 'k'}
[25717]  {:unknown, ' '}
[25718]  {:unknown, 'o'}
[25719]  {:unknown, 'u'}
[25720]  {:unknown, 't'}
[25721]  {:unknown, ' '}
[25722]  {:unknown, 't'}
[25723]  {:unknown, 'h'}
[25724]  {:unknown, 'e'}
[25725]  {:unknown, ' '}
[25726]  {:unknown, 's'}
[25727]  {:unknown, 'o'}
[25728]  {:unknown, 'l'}
[25729]  {:unknown, 'u'}
[25730]  {:unknown, 't'}
[25731]  {:unknown, 'i'}
[25732]  {:unknown, 'o'}
[25733]  {:unknown, 'n'}
[25734]  {:unknown, ' '}
[25735]  {:unknown, 't'}
[25736]  {:unknown, 'o'}
[25737]  {:unknown, ' '}
[25738]  {:unknown, 't'}
[25739]  {:unknown, 'h'}
[25740]  {:unknown, 'e'}
[25741]  {:unknown, ' '}
[25742]  {:unknown, 'p'}
[25743]  {:unknown, 'u'}
[25744]  {:unknown, 'z'}
[25745]  {:unknown, 'z'}
[25746]  {:unknown, 'l'}
[25747]  {:unknown, 'e'}
[25748]  {:unknown, '.'}
[25749]  {:unknown, ' '}
[25750]  {:unknown, ' '}
[25751]  {:unknown, 'I'}
[25752]  {:unknown, ' '}
[25753]  {:unknown, 'w'}
[25754]  {:unknown, 'i'}
[25755]  {:unknown, 'l'}
[25756]  {:unknown, 'l'}
[25757]  {:unknown, ' '}
[25758]  {:unknown, 'l'}
[25759]  {:unknown, 'e'}
[25760]  {:unknown, 'a'}
[25761]  {:unknown, 'v'}
[25762]  {:unknown, 'e'}
[25763]  {:unknown, ' '}
[25764]  {:unknown, 't'}
[25765]  {:unknown, 'h'}
[25766]  {:unknown, 'i'}
[25767]  {:unknown, 's'}
[25768]  {:unknown, ' '}
[25769]  {:unknown, 'j'}
[25770]  {:unknown, 'o'}
[25771]  {:unknown, 'u'}
[25772]  {:unknown, 'r'}
[25773]  {:unknown, 'n'}
[25774]  {:unknown, 'a'}
[25775]  {:unknown, 'l'}
[25776]  {:unknown, ' '}
[25777]  {:unknown, 'h'}
[25778]  {:unknown, 'e'}
[25779]  {:unknown, 'r'}
[25780]  {:unknown, 'e'}
[25781]  {:unknown, ' '}
[25782]  {:unknown, 't'}
[25783]  {:unknown, 'o'}
[25784]  {:unknown, ' '}
[25785]  {:unknown, 'h'}
[25786]  {:unknown, 'e'}
[25787]  {:unknown, 'l'}
[25788]  {:unknown, 'p'}
[25789]  {:unknown, ' '}
[25790]  {:unknown, 'f'}
[25791]  {:unknown, 'u'}
[25792]  {:unknown, 't'}
[25793]  {:unknown, 'u'}
[25794]  {:unknown, 'r'}
[25795]  {:unknown, 'e'}
[25796]  {:unknown, ' '}
[25797]  {:unknown, 'a'}
[25798]  {:unknown, 'd'}
[25799]  {:unknown, 'v'}
[25800]  {:unknown, 'e'}
[25801]  {:unknown, 'n'}
[25802]  {:unknown, 't'}
[25803]  {:unknown, 'u'}
[25804]  {:unknown, 'r'}
[25805]  {:unknown, 'e'}
[25806]  {:unknown, 'r'}
[25807]  {:unknown, 's'}
[25808]  {:unknown, ','}
[25809]  {:unknown, ' '}
[25810]  {:unknown, 't'}
[25811]  {:unknown, 'h'}
[25812]  {:unknown, 'o'}
[25813]  {:unknown, 'u'}
[25814]  {:unknown, 'g'}
[25815]  {:unknown, 'h'}
[25816]  {:unknown, ' '}
[25817]  {:unknown, 'I'}
[25818]  {:unknown, ' '}
[25819]  {:unknown, 'a'}
[25820]  {:unknown, 'm'}
[25821]  {:unknown, ' '}
[25822]  {:unknown, 'n'}
[25823]  {:unknown, 'o'}
[25824]  {:unknown, 't'}
[25825]  {:unknown, ' '}
[25826]  {:unknown, 's'}
[25827]  {:unknown, 'u'}
[25828]  {:unknown, 'r'}
[25829]  {:unknown, 'e'}
[25830]  {:unknown, ' '}
[25831]  {:unknown, 'w'}
[25832]  {:unknown, 'h'}
[25833]  {:unknown, 'a'}
[25834]  {:unknown, 't'}
[25835]  {:unknown, ' '}
[25836]  {:unknown, 'h'}
[25837]  {:unknown, 'e'}
[25838]  {:unknown, 'l'}
[25839]  {:unknown, 'p'}
[25840]  {:unknown, ' '}
[25841]  {:unknown, 'i'}
[25842]  {:unknown, 't'}
[25843]  {:unknown, ' '}
[25844]  {:unknown, 'w'}
[25845]  {:unknown, 'i'}
[25846]  {:unknown, 'l'}
[25847]  {:unknown, 'l'}
[25848]  {:unknown, ' '}
[25849]  {:unknown, 'g'}
[25850]  {:unknown, 'i'}
[25851]  {:unknown, 'v'}
[25852]  {:unknown, 'e'}
[25853]  {:unknown, '.'}
[25854]  {:unknown, ' '}
[25855]  {:unknown, ' '}
[25856]  {:unknown, 'G'}
[25857]  {:unknown, 'o'}
[25858]  {:unknown, 'o'}
[25859]  {:unknown, 'd'}
[25860]  {:unknown, ' '}
[25861]  {:unknown, 'l'}
[25862]  {:unknown, 'u'}
[25863]  {:unknown, 'c'}
[25864]  {:unknown, 'k'}
[25865]  {:unknown, '!'}
[25866]  {:unknown, '4'}
[25867]  {:unknown, 'a'}
[25868]  {:unknown, 'b'}
[25869]  {:unknown, 'c'}
[25870]  {:unknown, 'd'}
[25871]  {:unknown, 'e'}
[25872]  {:unknown, 'f'}
[25873]  {:unknown, 'g'}
[25874]  {:unknown, 'h'}
[25875]  {:unknown, 'i'}
[25876]  {:unknown, 'j'}
[25877]  {:unknown, 'k'}
[25878]  {:unknown, 'l'}
[25879]  {:unknown, 'm'}
[25880]  {:unknown, 'n'}
[25881]  {:unknown, 'o'}
[25882]  {:unknown, 'p'}
[25883]  {:unknown, 'q'}
[25884]  {:unknown, 'r'}
[25885]  {:unknown, 's'}
[25886]  {:unknown, 't'}
[25887]  {:unknown, 'u'}
[25888]  {:unknown, 'v'}
[25889]  {:unknown, 'w'}
[25890]  {:unknown, 'x'}
[25891]  {:unknown, 'y'}
[25892]  {:unknown, 'z'}
[25893]  {:unknown, 'A'}
[25894]  {:unknown, 'B'}
[25895]  {:unknown, 'C'}
[25896]  {:unknown, 'D'}
[25897]  {:unknown, 'E'}
[25898]  {:unknown, 'F'}
[25899]  {:unknown, 'G'}
[25900]  {:unknown, 'H'}
[25901]  {:unknown, 'I'}
[25902]  {:unknown, 'J'}
[25903]  {:unknown, 'K'}
[25904]  {:unknown, 'L'}
[25905]  {:unknown, 'M'}
[25906]  {:unknown, 'N'}
[25907]  {:unknown, 'O'}
[25908]  {:unknown, 'P'}
[25909]  {:unknown, 'Q'}
[25910]  {:unknown, 'R'}
[25911]  {:unknown, 'S'}
[25912]  {:unknown, 'T'}
[25913]  {:unknown, 'U'}
[25914]  {:unknown, 'V'}
[25915]  {:unknown, 'W'}
[25916]  {:unknown, 'X'}
[25917]  {:unknown, 'Y'}
[25918]  {:unknown, 'Z'}
[25919]  {:unknown, [23]}
[25920]  {:unknown, 'd'}
[25921]  {:unknown, 'b'}
[25922]  {:unknown, 'q'}
[25923]  {:unknown, 'p'}
[25924]  {:unknown, 'w'}
[25925]  {:unknown, 'u'}
[25926]  {:unknown, 'i'}
[25927]  {:unknown, 'o'}
[25928]  {:unknown, 'l'}
[25929]  {:unknown, 'x'}
[25930]  {:unknown, 'v'}
[25931]  {:unknown, '8'}
[25932]  {:unknown, 'W'}
[25933]  {:unknown, 'T'}
[25934]  {:unknown, 'Y'}
[25935]  {:unknown, 'U'}
[25936]  {:unknown, 'I'}
[25937]  {:unknown, 'O'}
[25938]  {:unknown, 'A'}
[25939]  {:unknown, 'H'}
[25940]  {:unknown, 'X'}
[25941]  {:unknown, 'V'}
[25942]  {:unknown, 'M'}
[25943]  {:push, {:value, 103}}
[25945]  {:unknown, 'o'}
[25946]  {:eq, {:value, 108}, {:value, 111}, {:value, 111}}
[25950]  {:unknown, 'k'}
[25951]  {:eq, {:value, 104}, {:value, 101}, {:value, 108}}
[25955]  {:unknown, 'p'}
[25956]  {:pop, {:value, 105}}
[25958]  {:unknown, 'n'}
[25959]  {:unknown, 'v'}
[25960]  {:eq, {:value, 116}, {:value, 97}, {:value, 107}}
[25964]  {:unknown, 'e'}
[25965]  {:eq, {:value, 100}, {:value, 114}, {:value, 111}}
[25969]  {:unknown, 'p'}
[25970]  {:pop, {:value, 117}}
[25972]  {:unknown, 's'}
[25973]  {:unknown, 'e'}
[25974]  {:halt}
[25975]  {:unknown, 'l'}
[25976]  {:unknown, 'o'}
[25977]  {:unknown, 'o'}
[25978]  {:unknown, 'k'}
[25979]  {:unknown, 't'}
[25980]  {:unknown, 'e'}
[25981]  {:unknown, 'l'}
[25982]  {:unknown, 'e'}
[25983]  {:unknown, 'p'}
[25984]  {:unknown, 'o'}
[25985]  {:unknown, 'r'}
[25986]  {:unknown, 't'}
[25987]  {:unknown, 'e'}
[25988]  {:unknown, 'r'}
[25989]  {:unknown, 'o'}
[25990]  {:unknown, 'o'}
[25991]  {:unknown, 'k'}
[25992]  {:unknown, 'd'}
[25993]  {:halt}
[25994]  {:halt}
[25995]  {:halt}
[25996]  {:halt}
[25997]  {:halt}
[25998]  {:halt}
[25999]  {:halt}
[26000]  {:halt}
[26001]  {:halt}
[26002]  {:halt}
[26003]  {:halt}
[26004]  {:halt}
[26005]  {:halt}
[26006]  {:halt}
[26007]  {:gt, {:value, 103}, {:value, 114}, {:value, 101}}
[26011]  {:unknown, 'e'}
[26012]  {:unknown, 'n'}
[26013]  {:pop, {:value, 114}}
[26015]  {:unknown, 'e'}
[26016]  {:unknown, 'd'}
[26017]  {:jmp, {:value, 121}}
[26019]  {:unknown, 'e'}
[26020]  {:unknown, 'l'}
[26021]  {:unknown, 'l'}
[26022]  {:unknown, 'o'}
[26023]  {:unknown, 'w'}
[26024]  {:unknown, '?'}
[26025]  {:unknown, 'A'}
[26026]  {:unknown, 's'}
[26027]  {:unknown, ' '}
[26028]  {:unknown, 'y'}
[26029]  {:unknown, 'o'}
[26030]  {:unknown, 'u'}
[26031]  {:unknown, ' '}
[26032]  {:unknown, 'e'}
[26033]  {:unknown, 'n'}
[26034]  {:unknown, 't'}
[26035]  {:unknown, 'e'}
[26036]  {:unknown, 'r'}
[26037]  {:unknown, ' '}
[26038]  {:unknown, 't'}
[26039]  {:unknown, 'h'}
[26040]  {:unknown, 'e'}
[26041]  {:unknown, ' '}
[26042]  {:unknown, 'r'}
[26043]  {:unknown, 'o'}
[26044]  {:unknown, 'o'}
[26045]  {:unknown, 'm'}
[26046]  {:unknown, ','}
[26047]  {:unknown, ' '}
[26048]  {:unknown, 't'}
[26049]  {:unknown, 'h'}
[26050]  {:unknown, 'e'}
[26051]  {:unknown, ' '}
[26052]  {:unknown, 's'}
[26053]  {:unknown, 'y'}
[26054]  {:unknown, 'm'}
[26055]  {:unknown, 'b'}
[26056]  {:unknown, 'o'}
[26057]  {:unknown, 'l'}
[26058]  {:unknown, ' '}
[26059]  {:unknown, 'o'}
[26060]  {:unknown, 'n'}
[26061]  {:unknown, ' '}
[26062]  {:unknown, 't'}
[26063]  {:unknown, 'h'}
[26064]  {:unknown, 'e'}
[26065]  {:unknown, ' '}
[26066]  {:unknown, 'f'}
[26067]  {:unknown, 'l'}
[26068]  {:unknown, 'o'}
[26069]  {:unknown, 'o'}
[26070]  {:unknown, 'r'}
[26071]  {:unknown, ' '}
[26072]  {:unknown, 'b'}
[26073]  {:unknown, 'r'}
[26074]  {:unknown, 'i'}
[26075]  {:unknown, 'e'}
[26076]  {:unknown, 'f'}
[26077]  {:unknown, 'l'}
[26078]  {:unknown, 'y'}
[26079]  {:unknown, ' '}
[26080]  {:unknown, 'f'}
[26081]  {:unknown, 'l'}
[26082]  {:unknown, 'a'}
[26083]  {:unknown, 's'}
[26084]  {:unknown, 'h'}
[26085]  {:unknown, 'e'}
[26086]  {:unknown, 's'}
[26087]  {:unknown, ' '}
[26088]  {:unknown, '!'}
[26089]  {:unknown, '.'}
[26090]  {:unknown, ' '}
[26091]  {:unknown, ' '}
[26092]  {:unknown, 'T'}
[26093]  {:unknown, 'h'}
[26094]  {:unknown, 'e'}
[26095]  {:unknown, ' '}
[26096]  {:unknown, 'o'}
[26097]  {:unknown, 'r'}
[26098]  {:unknown, 'b'}
[26099]  {:unknown, ' '}
[26100]  {:unknown, 'b'}
[26101]  {:unknown, 'e'}
[26102]  {:unknown, 'g'}
[26103]  {:unknown, 'i'}
[26104]  {:unknown, 'n'}
[26105]  {:unknown, 's'}
[26106]  {:unknown, ' '}
[26107]  {:unknown, 's'}
[26108]  {:unknown, 'u'}
[26109]  {:unknown, 'b'}
[26110]  {:unknown, 't'}
[26111]  {:unknown, 'l'}
[26112]  {:unknown, 'y'}
[26113]  {:unknown, ' '}
[26114]  {:unknown, 'g'}
[26115]  {:unknown, 'l'}
[26116]  {:unknown, 'o'}
[26117]  {:unknown, 'w'}
[26118]  {:unknown, 'i'}
[26119]  {:unknown, 'n'}
[26120]  {:unknown, 'g'}
[26121]  {:unknown, ' '}
[26122]  {:unknown, '/'}
[26123]  {:unknown, 'A'}
[26124]  {:unknown, 's'}
[26125]  {:unknown, ' '}
[26126]  {:unknown, 'y'}
[26127]  {:unknown, 'o'}
[26128]  {:unknown, 'u'}
[26129]  {:unknown, ' '}
[26130]  {:unknown, 'e'}
[26131]  {:unknown, 'n'}
[26132]  {:unknown, 't'}
[26133]  {:unknown, 'e'}
[26134]  {:unknown, 'r'}
[26135]  {:unknown, ' '}
[26136]  {:unknown, 't'}
[26137]  {:unknown, 'h'}
[26138]  {:unknown, 'e'}
[26139]  {:unknown, ' '}
[26140]  {:unknown, 'r'}
[26141]  {:unknown, 'o'}
[26142]  {:unknown, 'o'}
[26143]  {:unknown, 'm'}
[26144]  {:unknown, ','}
[26145]  {:unknown, ' '}
[26146]  {:unknown, 't'}
[26147]  {:unknown, 'h'}
[26148]  {:unknown, 'e'}
[26149]  {:unknown, ' '}
[26150]  {:unknown, 'o'}
[26151]  {:unknown, 'r'}
[26152]  {:unknown, 'b'}
[26153]  {:unknown, ' '}
[26154]  {:unknown, 'b'}
[26155]  {:unknown, 'r'}
[26156]  {:unknown, 'i'}
[26157]  {:unknown, 'e'}
[26158]  {:unknown, 'f'}
[26159]  {:unknown, 'l'}
[26160]  {:unknown, 'y'}
[26161]  {:unknown, ' '}
[26162]  {:unknown, 'f'}
[26163]  {:unknown, 'l'}
[26164]  {:unknown, 'a'}
[26165]  {:unknown, 's'}
[26166]  {:unknown, 'h'}
[26167]  {:unknown, 'e'}
[26168]  {:unknown, 's'}
[26169]  {:unknown, ' '}
[26170]  {:unknown, '@'}
[26171]  {:unknown, '.'}
[26172]  {:unknown, ' '}
[26173]  {:unknown, ' '}
[26174]  {:unknown, 'T'}
[26175]  {:unknown, 'h'}
[26176]  {:unknown, 'e'}
[26177]  {:unknown, ' '}
[26178]  {:unknown, 'n'}
[26179]  {:unknown, 'u'}
[26180]  {:unknown, 'm'}
[26181]  {:unknown, 'b'}
[26182]  {:unknown, 'e'}
[26183]  {:unknown, 'r'}
[26184]  {:unknown, ' '}
[26185]  {:unknown, 'o'}
[26186]  {:unknown, 'n'}
[26187]  {:unknown, ' '}
[26188]  {:unknown, 't'}
[26189]  {:unknown, 'h'}
[26190]  {:unknown, 'e'}
[26191]  {:unknown, ' '}
[26192]  {:unknown, 'f'}
[26193]  {:unknown, 'l'}
[26194]  {:unknown, 'o'}
[26195]  {:unknown, 'o'}
[26196]  {:unknown, 'r'}
[26197]  {:unknown, ' '}
[26198]  {:unknown, 'v'}
[26199]  {:unknown, 'i'}
[26200]  {:unknown, 'b'}
[26201]  {:unknown, 'r'}
[26202]  {:unknown, 'a'}
[26203]  {:unknown, 't'}
[26204]  {:unknown, 'e'}
[26205]  {:unknown, 's'}
[26206]  {:unknown, ' '}
[26207]  {:unknown, 's'}
[26208]  {:unknown, 't'}
[26209]  {:unknown, 'r'}
[26210]  {:unknown, 'a'}
[26211]  {:unknown, 'n'}
[26212]  {:unknown, 'g'}
[26213]  {:unknown, 'e'}
[26214]  {:unknown, 'l'}
[26215]  {:unknown, 'y'}
[26216]  {:unknown, ' '}
[26217]  {:unknown, 'b'}
[26218]  {:unknown, 'e'}
[26219]  {:unknown, 'n'}
[26220]  {:unknown, 'e'}
[26221]  {:unknown, 'a'}
[26222]  {:unknown, 't'}
[26223]  {:unknown, 'h'}
[26224]  {:unknown, ' '}
[26225]  {:unknown, 'y'}
[26226]  {:unknown, 'o'}
[26227]  {:unknown, 'u'}
[26228]  {:unknown, 'r'}
[26229]  {:unknown, ' '}
[26230]  {:unknown, 'f'}
[26231]  {:unknown, 'e'}
[26232]  {:unknown, 'e'}
[26233]  {:unknown, 't'}
[26234]  {:unknown, '.'}
[26235]  {:unknown, [31]}
[26236]  {:unknown, ' '}
[26237]  {:unknown, ' '}
[26238]  {:unknown, 'T'}
[26239]  {:unknown, 'h'}
[26240]  {:unknown, 'e'}
[26241]  {:unknown, ' '}
[26242]  {:unknown, 'o'}
[26243]  {:unknown, 'r'}
[26244]  {:unknown, 'b'}
[26245]  {:unknown, ' '}
[26246]  {:unknown, 's'}
[26247]  {:unknown, 'e'}
[26248]  {:unknown, 'e'}
[26249]  {:unknown, 'm'}
[26250]  {:unknown, 's'}
[26251]  {:unknown, ' '}
[26252]  {:unknown, 't'}
[26253]  {:unknown, 'o'}
[26254]  {:unknown, ' '}
[26255]  {:unknown, 'g'}
[26256]  {:unknown, 'e'}
[26257]  {:unknown, 't'}
[26258]  {:unknown, ' '}
[26259]  {:unknown, 'h'}
[26260]  {:unknown, 'e'}
[26261]  {:unknown, 'a'}
[26262]  {:unknown, 'v'}
[26263]  {:unknown, 'i'}
[26264]  {:unknown, 'e'}
[26265]  {:unknown, 'r'}
[26266]  {:unknown, '.'}
[26267]  {:unknown, [31]}
[26268]  {:unknown, ' '}
[26269]  {:unknown, ' '}
[26270]  {:unknown, 'T'}
[26271]  {:unknown, 'h'}
[26272]  {:unknown, 'e'}
[26273]  {:unknown, ' '}
[26274]  {:unknown, 'o'}
[26275]  {:unknown, 'r'}
[26276]  {:unknown, 'b'}
[26277]  {:unknown, ' '}
[26278]  {:unknown, 's'}
[26279]  {:unknown, 'e'}
[26280]  {:unknown, 'e'}
[26281]  {:unknown, 'm'}
[26282]  {:unknown, 's'}
[26283]  {:unknown, ' '}
[26284]  {:unknown, 't'}
[26285]  {:unknown, 'o'}
[26286]  {:unknown, ' '}
[26287]  {:unknown, 'g'}
[26288]  {:unknown, 'e'}
[26289]  {:unknown, 't'}
[26290]  {:unknown, ' '}
[26291]  {:unknown, 'l'}
[26292]  {:unknown, 'i'}
[26293]  {:unknown, 'g'}
[26294]  {:unknown, 'h'}
[26295]  {:unknown, 't'}
[26296]  {:unknown, 'e'}
[26297]  {:unknown, 'r'}
[26298]  {:unknown, '.'}
[26299]  {:noop}
[26300]  {:unknown, ' '}
[26301]  {:unknown, ' '}
[26302]  {:unknown, 'T'}
[26303]  {:unknown, 'h'}
[26304]  {:unknown, 'e'}
[26305]  {:unknown, ' '}
[26306]  {:unknown, 'o'}
[26307]  {:unknown, 'r'}
[26308]  {:unknown, 'b'}
[26309]  {:unknown, ' '}
[26310]  {:unknown, 's'}
[26311]  {:unknown, 'h'}
[26312]  {:unknown, 'a'}
[26313]  {:unknown, 't'}
[26314]  {:unknown, 't'}
[26315]  {:unknown, 'e'}
[26316]  {:unknown, 'r'}
[26317]  {:unknown, 's'}
[26318]  {:unknown, '!'}
[26319]  {:mult, {:value, 10}, {:value, 32}, {:value, 65}}
[26323]  {:unknown, 's'}
[26324]  {:unknown, ' '}
[26325]  {:unknown, 'y'}
[26326]  {:unknown, 'o'}
[26327]  {:unknown, 'u'}
[26328]  {:unknown, ' '}
[26329]  {:unknown, 'a'}
[26330]  {:unknown, 'p'}
[26331]  {:unknown, 'p'}
[26332]  {:unknown, 'r'}
[26333]  {:unknown, 'o'}
[26334]  {:unknown, 'a'}
[26335]  {:unknown, 'c'}
[26336]  {:unknown, 'h'}
[26337]  {:unknown, ' '}
[26338]  {:unknown, 't'}
[26339]  {:unknown, 'h'}
[26340]  {:unknown, 'e'}
[26341]  {:unknown, ' '}
[26342]  {:unknown, 'v'}
[26343]  {:unknown, 'a'}
[26344]  {:unknown, 'u'}
[26345]  {:unknown, 'l'}
[26346]  {:unknown, 't'}
[26347]  {:unknown, ' '}
[26348]  {:unknown, 'd'}
[26349]  {:unknown, 'o'}
[26350]  {:unknown, 'o'}
[26351]  {:unknown, 'r'}
[26352]  {:unknown, ','}
[26353]  {:unknown, ' '}
[26354]  {:unknown, '+'}
[26355]  {:unknown, 't'}
[26356]  {:unknown, 'h'}
[26357]  {:unknown, 'e'}
[26358]  {:unknown, ' '}
[26359]  {:unknown, 'n'}
[26360]  {:unknown, 'u'}
[26361]  {:unknown, 'm'}
[26362]  {:unknown, 'b'}
[26363]  {:unknown, 'e'}
[26364]  {:unknown, 'r'}
[26365]  {:unknown, ' '}
[26366]  {:unknown, 'o'}
[26367]  {:unknown, 'n'}
[26368]  {:unknown, ' '}
[26369]  {:unknown, 't'}
[26370]  {:unknown, 'h'}
[26371]  {:unknown, 'e'}
[26372]  {:unknown, ' '}
[26373]  {:unknown, 'v'}
[26374]  {:unknown, 'a'}
[26375]  {:unknown, 'u'}
[26376]  {:unknown, 'l'}
[26377]  {:unknown, 't'}
[26378]  {:unknown, ' '}
[26379]  {:unknown, 'd'}
[26380]  {:unknown, 'o'}
[26381]  {:unknown, 'o'}
[26382]  {:unknown, 'r'}
[26383]  {:unknown, ' '}
[26384]  {:unknown, 'f'}
[26385]  {:unknown, 'l'}
[26386]  {:unknown, 'a'}
[26387]  {:unknown, 's'}
[26388]  {:unknown, 'h'}
[26389]  {:unknown, 'e'}
[26390]  {:unknown, 's'}
[26391]  {:unknown, ' '}
[26392]  {:unknown, 'b'}
[26393]  {:unknown, 'l'}
[26394]  {:unknown, 'a'}
[26395]  {:unknown, 'c'}
[26396]  {:unknown, 'k'}
[26397]  {:unknown, '.'}
[26398]  {:unknown, ')'}
[26399]  {:unknown, ' '}
[26400]  {:unknown, ' '}
[26401]  {:unknown, 'T'}
[26402]  {:unknown, 'h'}
[26403]  {:unknown, 'e'}
[26404]  {:unknown, ' '}
[26405]  {:unknown, 'o'}
[26406]  {:unknown, 'r'}
[26407]  {:unknown, 'b'}
[26408]  {:unknown, ' '}
[26409]  {:unknown, 'e'}
[26410]  {:unknown, 'v'}
[26411]  {:unknown, 'a'}
[26412]  {:unknown, 'p'}
[26413]  {:unknown, 'o'}
[26414]  {:unknown, 'r'}
[26415]  {:unknown, 'a'}
[26416]  {:unknown, 't'}
[26417]  {:unknown, 'e'}
[26418]  {:unknown, 's'}
[26419]  {:unknown, ' '}
[26420]  {:unknown, 'o'}
[26421]  {:unknown, 'u'}
[26422]  {:unknown, 't'}
[26423]  {:unknown, ' '}
[26424]  {:unknown, 'o'}
[26425]  {:unknown, 'f'}
[26426]  {:unknown, ' '}
[26427]  {:unknown, 'y'}
[26428]  {:unknown, 'o'}
[26429]  {:unknown, 'u'}
[26430]  {:unknown, 'r'}
[26431]  {:unknown, ' '}
[26432]  {:unknown, 'h'}
[26433]  {:unknown, 'a'}
[26434]  {:unknown, 'n'}
[26435]  {:unknown, 'd'}
[26436]  {:unknown, 's'}
[26437]  {:unknown, '.'}
[26438]  {:mult, {:value, 10}, {:value, 43}, {:value, 116}}
[26442]  {:unknown, 'h'}
[26443]  {:unknown, 'e'}
[26444]  {:unknown, ' '}
[26445]  {:unknown, 'n'}
[26446]  {:unknown, 'u'}
[26447]  {:unknown, 'm'}
[26448]  {:unknown, 'b'}
[26449]  {:unknown, 'e'}
[26450]  {:unknown, 'r'}
[26451]  {:unknown, ' '}
[26452]  {:unknown, 'o'}
[26453]  {:unknown, 'n'}
[26454]  {:unknown, ' '}
[26455]  {:unknown, 't'}
[26456]  {:unknown, 'h'}
[26457]  {:unknown, 'e'}
[26458]  {:unknown, ' '}
[26459]  {:unknown, 'v'}
[26460]  {:unknown, 'a'}
[26461]  {:unknown, 'u'}
[26462]  {:unknown, 'l'}
[26463]  {:unknown, 't'}
[26464]  {:unknown, ' '}
[26465]  {:unknown, 'd'}
[26466]  {:unknown, 'o'}
[26467]  {:unknown, 'o'}
[26468]  {:unknown, 'r'}
[26469]  {:unknown, ' '}
[26470]  {:unknown, 'f'}
[26471]  {:unknown, 'l'}
[26472]  {:unknown, 'a'}
[26473]  {:unknown, 's'}
[26474]  {:unknown, 'h'}
[26475]  {:unknown, 'e'}
[26476]  {:unknown, 's'}
[26477]  {:unknown, ' '}
[26478]  {:unknown, 'w'}
[26479]  {:unknown, 'h'}
[26480]  {:unknown, 'i'}
[26481]  {:unknown, 't'}
[26482]  {:unknown, 'e'}
[26483]  {:unknown, '!'}
[26484]  {:unknown, 'c'}
[26485]  {:unknown, ' '}
[26486]  {:unknown, ' '}
[26487]  {:unknown, 'T'}
[26488]  {:unknown, 'h'}
[26489]  {:unknown, 'e'}
[26490]  {:unknown, ' '}
[26491]  {:unknown, 'h'}
[26492]  {:unknown, 'o'}
[26493]  {:unknown, 'u'}
[26494]  {:unknown, 'r'}
[26495]  {:unknown, 'g'}
[26496]  {:unknown, 'l'}
[26497]  {:unknown, 'a'}
[26498]  {:unknown, 's'}
[26499]  {:unknown, 's'}
[26500]  {:unknown, ' '}
[26501]  {:unknown, 'h'}
[26502]  {:unknown, 'a'}
[26503]  {:unknown, 's'}
[26504]  {:unknown, ' '}
[26505]  {:unknown, 'a'}
[26506]  {:unknown, 'l'}
[26507]  {:unknown, 'r'}
[26508]  {:unknown, 'e'}
[26509]  {:unknown, 'a'}
[26510]  {:unknown, 'd'}
[26511]  {:unknown, 'y'}
[26512]  {:unknown, ' '}
[26513]  {:unknown, 'r'}
[26514]  {:unknown, 'u'}
[26515]  {:unknown, 'n'}
[26516]  {:unknown, ' '}
[26517]  {:unknown, 'o'}
[26518]  {:unknown, 'u'}
[26519]  {:unknown, 't'}
[26520]  {:unknown, '.'}
[26521]  {:unknown, ' '}
[26522]  {:unknown, ' '}
[26523]  {:unknown, 'I'}
[26524]  {:unknown, 't'}
[26525]  {:unknown, ' '}
[26526]  {:unknown, 'f'}
[26527]  {:unknown, 'l'}
[26528]  {:unknown, 'a'}
[26529]  {:unknown, 's'}
[26530]  {:unknown, 'h'}
[26531]  {:unknown, 'e'}
[26532]  {:unknown, 's'}
[26533]  {:unknown, ' '}
[26534]  {:unknown, 'b'}
[26535]  {:unknown, 'l'}
[26536]  {:unknown, 'a'}
[26537]  {:unknown, 'c'}
[26538]  {:unknown, 'k'}
[26539]  {:unknown, ' '}
[26540]  {:unknown, 'a'}
[26541]  {:unknown, 'n'}
[26542]  {:unknown, 'd'}
[26543]  {:unknown, ' '}
[26544]  {:unknown, 'f'}
[26545]  {:unknown, 'l'}
[26546]  {:unknown, 'i'}
[26547]  {:unknown, 'p'}
[26548]  {:unknown, 's'}
[26549]  {:unknown, ' '}
[26550]  {:unknown, 'o'}
[26551]  {:unknown, 'v'}
[26552]  {:unknown, 'e'}
[26553]  {:unknown, 'r'}
[26554]  {:unknown, ','}
[26555]  {:unknown, ' '}
[26556]  {:unknown, 'r'}
[26557]  {:unknown, 'e'}
[26558]  {:unknown, 's'}
[26559]  {:unknown, 't'}
[26560]  {:unknown, 'a'}
[26561]  {:unknown, 'r'}
[26562]  {:unknown, 't'}
[26563]  {:unknown, 'i'}
[26564]  {:unknown, 'n'}
[26565]  {:unknown, 'g'}
[26566]  {:unknown, ' '}
[26567]  {:unknown, 't'}
[26568]  {:unknown, 'h'}
[26569]  {:unknown, 'e'}
[26570]  {:unknown, ' '}
[26571]  {:unknown, 'f'}
[26572]  {:unknown, 'l'}
[26573]  {:unknown, 'o'}
[26574]  {:unknown, 'w'}
[26575]  {:unknown, ' '}
[26576]  {:unknown, 'o'}
[26577]  {:unknown, 'f'}
[26578]  {:unknown, ' '}
[26579]  {:unknown, 's'}
[26580]  {:unknown, 'a'}
[26581]  {:unknown, 'n'}
[26582]  {:unknown, 'd'}
[26583]  {:unknown, '.'}
[26584]  {:unknown, [132]}
[26585]  {:unknown, ' '}
[26586]  {:unknown, ' '}
[26587]  {:unknown, 'T'}
[26588]  {:unknown, 'h'}
[26589]  {:unknown, 'e'}
[26590]  {:unknown, ' '}
[26591]  {:unknown, 'h'}
[26592]  {:unknown, 'o'}
[26593]  {:unknown, 'u'}
[26594]  {:unknown, 'r'}
[26595]  {:unknown, 'g'}
[26596]  {:unknown, 'l'}
[26597]  {:unknown, 'a'}
[26598]  {:unknown, 's'}
[26599]  {:unknown, 's'}
[26600]  {:unknown, ' '}
[26601]  {:unknown, 'i'}
[26602]  {:unknown, 's'}
[26603]  {:unknown, ' '}
[26604]  {:unknown, 's'}
[26605]  {:unknown, 't'}
[26606]  {:unknown, 'i'}
[26607]  {:unknown, 'l'}
[26608]  {:unknown, 'l'}
[26609]  {:unknown, ' '}
[26610]  {:unknown, 'r'}
[26611]  {:unknown, 'u'}
[26612]  {:unknown, 'n'}
[26613]  {:unknown, 'n'}
[26614]  {:unknown, 'i'}
[26615]  {:unknown, 'n'}
[26616]  {:unknown, 'g'}
[26617]  {:unknown, '!'}
[26618]  {:unknown, ' '}
[26619]  {:unknown, ' '}
[26620]  {:unknown, 'I'}
[26621]  {:unknown, 't'}
[26622]  {:unknown, ' '}
[26623]  {:unknown, 'f'}
[26624]  {:unknown, 'l'}
[26625]  {:unknown, 'a'}
[26626]  {:unknown, 's'}
[26627]  {:unknown, 'h'}
[26628]  {:unknown, 'e'}
[26629]  {:unknown, 's'}
[26630]  {:unknown, ' '}
[26631]  {:unknown, 'w'}
[26632]  {:unknown, 'h'}
[26633]  {:unknown, 'i'}
[26634]  {:unknown, 't'}
[26635]  {:unknown, 'e'}
[26636]  {:unknown, '!'}
[26637]  {:unknown, ' '}
[26638]  {:unknown, ' '}
[26639]  {:unknown, 'Y'}
[26640]  {:unknown, 'o'}
[26641]  {:unknown, 'u'}
[26642]  {:unknown, ' '}
[26643]  {:unknown, 'h'}
[26644]  {:unknown, 'e'}
[26645]  {:unknown, 'a'}
[26646]  {:unknown, 'r'}
[26647]  {:unknown, ' '}
[26648]  {:unknown, 'a'}
[26649]  {:unknown, ' '}
[26650]  {:unknown, 'c'}
[26651]  {:unknown, 'l'}
[26652]  {:unknown, 'i'}
[26653]  {:unknown, 'c'}
[26654]  {:unknown, 'k'}
[26655]  {:unknown, ' '}
[26656]  {:unknown, 'f'}
[26657]  {:unknown, 'r'}
[26658]  {:unknown, 'o'}
[26659]  {:unknown, 'm'}
[26660]  {:unknown, ' '}
[26661]  {:unknown, 't'}
[26662]  {:unknown, 'h'}
[26663]  {:unknown, 'e'}
[26664]  {:unknown, ' '}
[26665]  {:unknown, 'v'}
[26666]  {:unknown, 'a'}
[26667]  {:unknown, 'u'}
[26668]  {:unknown, 'l'}
[26669]  {:unknown, 't'}
[26670]  {:unknown, ' '}
[26671]  {:unknown, 'd'}
[26672]  {:unknown, 'o'}
[26673]  {:unknown, 'o'}
[26674]  {:unknown, 'r'}
[26675]  {:unknown, '.'}
[26676]  {:unknown, ' '}
[26677]  {:unknown, ' '}
[26678]  {:unknown, 'T'}
[26679]  {:unknown, 'h'}
[26680]  {:unknown, 'e'}
[26681]  {:unknown, ' '}
[26682]  {:unknown, 'o'}
[26683]  {:unknown, 'r'}
[26684]  {:unknown, 'b'}
[26685]  {:unknown, ' '}
[26686]  {:unknown, 'e'}
[26687]  {:unknown, 'v'}
[26688]  {:unknown, 'a'}
[26689]  {:unknown, 'p'}
[26690]  {:unknown, 'o'}
[26691]  {:unknown, 'r'}
[26692]  {:unknown, 'a'}
[26693]  {:unknown, 't'}
[26694]  {:unknown, 'e'}
[26695]  {:unknown, 's'}
[26696]  {:unknown, ' '}
[26697]  {:unknown, 'o'}
[26698]  {:unknown, 'u'}
[26699]  {:unknown, 't'}
[26700]  {:unknown, ' '}
[26701]  {:unknown, 'o'}
[26702]  {:unknown, 'f'}
[26703]  {:unknown, ' '}
[26704]  {:unknown, 'h'}
[26705]  {:unknown, 'o'}
[26706]  {:unknown, 'u'}
[26707]  {:unknown, 'r'}
[26708]  {:unknown, ' '}
[26709]  {:unknown, 'h'}
[26710]  {:unknown, 'a'}
[26711]  {:unknown, 'n'}
[26712]  {:unknown, 'd'}
[26713]  {:unknown, 's'}
[26714]  {:unknown, '.'}
[26715]  {:mult, {:value, 10}, {:value, 7}, {:value, 65}}
[26719]  {:unknown, 's'}
[26720]  {:unknown, ' '}
[26721]  {:unknown, 'y'}
[26722]  {:unknown, 'o'}
[26723]  {:unknown, 'u'}
[26724]  {:unknown, ' '}
[26725]  {:gt, {:value, 108}, {:value, 101}, {:value, 97}}
[26729]  {:unknown, 'v'}
[26730]  {:unknown, 'e'}
[26731]  {:gt, {:value, 101}, {:value, 110}, {:value, 116}}
[26735]  {:unknown, 'e'}
[26736]  {:unknown, 'r'}
[26737]  {:unknown, 'V'}
[26738]  {:unknown, ' '}
[26739]  {:unknown, 't'}
[26740]  {:unknown, 'h'}
[26741]  {:unknown, 'e'}
[26742]  {:unknown, ' '}
[26743]  {:unknown, 'r'}
[26744]  {:unknown, 'o'}
[26745]  {:unknown, 'o'}
[26746]  {:unknown, 'm'}
[26747]  {:unknown, ','}
[26748]  {:unknown, ' '}
[26749]  {:unknown, 't'}
[26750]  {:unknown, 'h'}
[26751]  {:unknown, 'e'}
[26752]  {:unknown, ' '}
[26753]  {:unknown, 'o'}
[26754]  {:unknown, 'r'}
[26755]  {:unknown, 'b'}
[26756]  {:unknown, ' '}
[26757]  {:unknown, 'e'}
[26758]  {:unknown, 'v'}
[26759]  {:unknown, 'a'}
[26760]  {:unknown, 'p'}
[26761]  {:unknown, 'o'}
[26762]  {:unknown, 'r'}
[26763]  {:unknown, 'a'}
[26764]  {:unknown, 't'}
[26765]  {:unknown, 'e'}
[26766]  {:unknown, 's'}
[26767]  {:unknown, ' '}
[26768]  {:unknown, 'o'}
[26769]  {:unknown, 'u'}
[26770]  {:unknown, 't'}
[26771]  {:unknown, ' '}
[26772]  {:unknown, 'o'}
[26773]  {:unknown, 'f'}
[26774]  {:unknown, ' '}
[26775]  {:unknown, 'y'}
[26776]  {:unknown, 'o'}
[26777]  {:unknown, 'u'}
[26778]  {:unknown, 'r'}
[26779]  {:unknown, ' '}
[26780]  {:unknown, 'h'}
[26781]  {:unknown, 'a'}
[26782]  {:unknown, 'n'}
[26783]  {:unknown, 'd'}
[26784]  {:unknown, 's'}
[26785]  {:unknown, '!'}
[26786]  {:unknown, ' '}
[26787]  {:unknown, ' '}
[26788]  {:unknown, 'I'}
[26789]  {:unknown, 't'}
[26790]  {:unknown, ' '}
[26791]  {:unknown, 'r'}
[26792]  {:unknown, 'e'}
[26793]  {:unknown, 'm'}
[26794]  {:unknown, 'a'}
[26795]  {:unknown, 't'}
[26796]  {:unknown, 'e'}
[26797]  {:unknown, 'r'}
[26798]  {:unknown, 'i'}
[26799]  {:unknown, 'a'}
[26800]  {:unknown, 'l'}
[26801]  {:unknown, 'i'}
[26802]  {:unknown, 'z'}
[26803]  {:unknown, 'e'}
[26804]  {:unknown, 's'}
[26805]  {:unknown, ' '}
[26806]  {:unknown, 'o'}
[26807]  {:unknown, 'n'}
[26808]  {:unknown, ' '}
[26809]  {:unknown, 'i'}
[26810]  {:unknown, 't'}
[26811]  {:unknown, 's'}
[26812]  {:unknown, ' '}
[26813]  {:unknown, 'p'}
[26814]  {:unknown, 'e'}
[26815]  {:unknown, 'd'}
[26816]  {:unknown, 'e'}
[26817]  {:unknown, 's'}
[26818]  {:unknown, 't'}
[26819]  {:unknown, 'a'}
[26820]  {:unknown, 'l'}
[26821]  {:unknown, '.'}
[26822]  {:mult, {:value, 10}, {:value, 26}, {:value, 84}}
[26826]  {:unknown, 'h'}
[26827]  {:unknown, 'e'}
[26828]  {:unknown, ' '}
[26829]  {:unknown, 'v'}
[26830]  {:unknown, 'a'}
[26831]  {:unknown, 'u'}
[26832]  {:unknown, 'l'}
[26833]  {:unknown, 't'}
[26834]  {:unknown, ' '}
[26835]  {:unknown, 'd'}
[26836]  {:unknown, 'o'}
[26837]  {:unknown, 'o'}
[26838]  {:unknown, 'r'}
[26839]  {:unknown, ' '}
[26840]  {:unknown, 'i'}
[26841]  {:unknown, 's'}
[26842]  {:unknown, ' '}
[26843]  {:unknown, 's'}
[26844]  {:unknown, 'e'}
[26845]  {:unknown, 'a'}
[26846]  {:unknown, 'l'}
[26847]  {:unknown, 'e'}
[26848]  {:unknown, 'd'}
[26849]  {:unknown, '.'}
[26850]  {:mult, {:value, 48}, {:value, 21935}, {:value, 21907}}
[26854]  {:unknown, [21918]}
[26855]  {:unknown, [21979]}
[26856]  {:unknown, [21896]}
[26857]  {:unknown, [21918]}
[26858]  {:unknown, [21911]}
[26859]  {:unknown, [21917]}
[26860]  {:unknown, [21974]}
[26861]  {:unknown, [21903]}
[26862]  {:unknown, [21918]}
[26863]  {:unknown, [21896]}
[26864]  {:unknown, [21903]}
[26865]  {:unknown, [21979]}
[26866]  {:unknown, [21912]}
[26867]  {:unknown, [21908]}
[26868]  {:unknown, [21910]}
[26869]  {:unknown, [21899]}
[26870]  {:unknown, [21911]}
[26871]  {:unknown, [21918]}
[26872]  {:unknown, [21903]}
[26873]  {:unknown, [21906]}
[26874]  {:unknown, [21908]}
[26875]  {:unknown, [21909]}
[26876]  {:unknown, [21979]}
[26877]  {:unknown, [21912]}
[26878]  {:unknown, [21908]}
[26879]  {:unknown, [21919]}
[26880]  {:unknown, [21918]}
[26881]  {:unknown, [21979]}
[26882]  {:unknown, [21906]}
[26883]  {:unknown, [21896]}
[26884]  {:unknown, [21953]}
[26885]  {:unknown, [21979]}
[26886]  {:unknown, [21899]}
[26887]  {:unknown, [21950]}
[26888]  {:unknown, [21899]}
[26889]  {:unknown, [21932]}
[26890]  {:unknown, [21903]}
[26891]  {:unknown, [21914]}
[26892]  {:unknown, [21935]}
[26893]  {:unknown, [21910]}
[26894]  {:unknown, [21913]}
[26895]  {:unknown, [21934]}
[26896]  {:unknown, [21901]}
[26897]  {:unknown, [21908]}
[26898]  {:unknown, [22001]}
[26899]  {:unknown, [22001]}
[26900]  {:push, {:value, 6336}}
[26902]  {:unknown, [6344]}
[26903]  {:push, {:value, 2327}}
[26905]  {:unknown, [2322]}
[26906]  {:set, {:value, 6435}, {:value, 1}}
[26909]  {:unknown, [2317]}
[26910]  {:push, {:value, 6585}}
[26912]  {:unknown, [6591]}
[26913]  {:push, {:value, 2332}}
[26915]  {:unknown, [2317]}
[26916]  {:push, {:value, 6699}}
[26918]  {:unknown, [6705]}
[26919]  {:push, {:value, 2337}}
[26921]  {:unknown, [2327]}
[26922]  {:push, {:value, 6909}}
[26924]  {:unknown, [6916]}
[26925]  {:push, {:value, 2342}}
[26927]  {:unknown, [2327]}
[26928]  {:push, {:value, 7056}}
[26930]  {:unknown, [7065]}
[26931]  {:push, {:value, 2347}}
[26933]  {:unknown, [2337]}
[26934]  {:set, {:value, 7265}, {:value, 1}}
[26937]  {:unknown, [2352]}
[26938]  {:push, {:value, 7492}}
[26940]  {:unknown, [7497]}
[26941]  {:push, {:value, 2362}}
[26943]  {:unknown, [2357]}
[26944]  {:set, {:value, 7611}, {:value, 1}}
[26947]  {:unknown, [2352]}
[26948]  {:push, {:value, 7786}}
[26950]  {:unknown, [7791]}
[26951]  {:push, {:value, 2352}}
[26953]  {:unknown, [2367]}
[26954]  {:pop, {:value, 7984}}
[26956]  {:unknown, [7991]}
[26957]  {:unknown, [7998]}
[26958]  {:pop, {:value, 2362}}
[26960]  {:unknown, [2377]}
[26961]  {:unknown, [2372]}
[26962]  {:push, {:value, 8015}}
[26964]  {:unknown, [8073]}
[26965]  {:push, {:value, 8149}}
[26967]  {:unknown, [8158]}
[26968]  {:push, {:value, 2427}}
[26970]  {:unknown, [2367]}
[26971]  {:gt, {:value, 8303}, {:value, 8310}, {:value, 8316}}
[26975]  {:unknown, [8322]}
[26976]  {:unknown, [8327]}
[26977]  {:gt, {:value, 2367}, {:value, 2382}, {:value, 2387}}
[26981]  {:unknown, [2392]}
[26982]  {:unknown, [2397]}
[26983]  {:pop, {:value, 8404}}
[26985]  {:unknown, [8410]}
[26986]  {:unknown, [8416]}
[26987]  {:pop, {:value, 2387}}
[26989]  {:unknown, [2377]}
[26990]  {:unknown, [2382]}
[26991]  {:pop, {:value, 8493}}
[26993]  {:unknown, [8499]}
[26994]  {:unknown, [8505]}
[26995]  {:pop, {:value, 2377}}
[26997]  {:unknown, [2382]}
[26998]  {:unknown, [2387]}
[26999]  {:eq, {:value, 8651}, {:value, 8657}, {:value, 8663}}
[27003]  {:unknown, [8668]}
[27004]  {:eq, {:value, 2407}, {:value, 2392}, {:value, 2377}}
[27008]  {:unknown, [2422]}
[27009]  {:pop, {:value, 8745}}
[27011]  {:unknown, [8751]}
[27012]  {:unknown, [8757]}
[27013]  {:pop, {:value, 2397}}
[27015]  {:unknown, [2402]}
[27016]  {:unknown, [2377]}
[27017]  {:eq, {:value, 8910}, {:value, 8916}, {:value, 8922}}
[27021]  {:unknown, [8927]}
[27022]  {:eq, {:value, 2412}, {:value, 2382}, {:value, 2387}}
[27026]  {:unknown, [2422]}
[27027]  {:pop, {:value, 9004}}
[27029]  {:unknown, [9010]}
[27030]  {:unknown, [9015]}
[27031]  {:pop, {:value, 2387}}
[27033]  {:unknown, [2382]}
[27034]  {:unknown, [2407]}
[27035]  {:set, {:value, 9093}, {:value, 1}}
[27038]  {:unknown, [2377]}
[27039]  {:set, {:value, 9170}, {:value, 1}}
[27042]  {:unknown, [2377]}
[27043]  {:push, {:value, 9247}}
[27045]  {:unknown, [9253]}
[27046]  {:push, {:value, 2387}}
[27048]  {:unknown, [2382]}
[27049]  {:push, {:value, 9387}}
[27051]  {:unknown, [9392]}
[27052]  {:push, {:value, 2432}}
[27054]  {:unknown, [2372]}
[27055]  {:push, {:value, 9445}}
[27057]  {:unknown, [9450]}
[27058]  {:push, {:value, 2427}}
[27060]  {:unknown, [2437]}
[27061]  {:push, {:value, 9503}}
[27063]  {:unknown, [9508]}
[27064]  {:push, {:value, 2432}}
[27066]  {:unknown, [2442]}
[27067]  {:push, {:value, 9627}}
[27069]  {:unknown, [9632]}
[27070]  {:push, {:value, 2437}}
[27072]  {:unknown, [2447]}
[27073]  {:push, {:value, 9892}}
[27075]  {:unknown, [9897]}
[27076]  {:push, {:value, 2442}}
[27078]  {:unknown, [2452]}
[27079]  {:push, {:value, 10091}}
[27081]  {:unknown, [10097]}
[27082]  {:push, {:value, 2457}}
[27084]  {:unknown, [2447]}
[27085]  {:eq, {:value, 10403}, {:value, 10409}, {:value, 10415}}
[27089]  {:unknown, [10420]}
[27090]  {:eq, {:value, 2463}, {:value, 2452}, {:value, 2468}}
[27094]  {:unknown, [2478]}
[27095]  {:gt, {:value, 267}, {:value, 271}, {:value, 275}}
[27099]  {:unknown, [281]}
[27100]  {:unknown, [287]}
[27101]  {:gt, {:value, 0}, {:value, 0}, {:value, 0}}
[27105]  {:halt}
[27106]  {:halt}
[27107]  {:set, {:value, 10599}, {:value, 1}}
[27110]  {:unknown, [2457]}
[27111]  {:push, {:value, 10750}}
[27113]  {:unknown, [10755]}
[27114]  {:push, {:value, 2473}}
[27116]  {:unknown, [2457]}
[27117]  {:set, {:value, 10889}, {:value, 1}}
[27120]  {:unknown, [2468]}
[27121]  {:push, {:value, 11078}}
[27123]  {:unknown, [11081]}
[27124]  {:push, {:value, 2483}}
[27126]  {:unknown, [2457]}
[27127]  {:set, {:value, 11262}, {:value, 1}}
[27130]  {:unknown, [2478]}
[27131]  {:set, {:value, 11544}, {:value, 1}}
[27134]  {:unknown, [2493]}
[27135]  {:set, {:value, 11719}, {:value, 1}}
[27138]  {:unknown, [2488]}
[27139]  {:pop, {:value, 11903}}
[27141]  {:unknown, [11908]}
[27142]  {:unknown, [11913]}
[27143]  {:pop, {:value, 2503}}
[27145]  {:unknown, [2508]}
[27146]  {:unknown, [2513]}
[27147]  {:push, {:value, 12188}}
[27149]  {:unknown, [12193]}
[27150]  {:push, {:value, 2498}}
[27152]  {:unknown, [2513]}
[27153]  {:push, {:value, 12448}}
[27155]  {:unknown, [12453]}
[27156]  {:push, {:value, 2498}}
[27158]  {:unknown, [2518]}
[27159]  {:pop, {:value, 12674}}
[27161]  {:unknown, [12680]}
[27162]  {:unknown, [12686]}
[27163]  {:pop, {:value, 2523}}
[27165]  {:unknown, [2498]}
[27166]  {:unknown, [2518]}
[27167]  {:pop, {:value, 12830}}
[27169]  {:unknown, [12836]}
[27170]  {:unknown, [12842]}
[27171]  {:pop, {:value, 2523}}
[27173]  {:unknown, [2508]}
[27174]  {:unknown, [2513]}
[27175]  {:push, {:value, 13094}}
[27177]  {:unknown, [13100]}
[27178]  {:push, {:value, 2528}}
[27180]  {:unknown, [2513]}
[27181]  {:push, {:value, 13408}}
[27183]  {:unknown, [13414]}
[27184]  {:push, {:value, 2533}}
[27186]  {:unknown, [2523]}
[27187]  {:push, {:value, 13682}}
[27189]  {:unknown, [13688]}
[27190]  {:push, {:value, 2538}}
[27192]  {:unknown, [2528]}
[27193]  {:push, {:value, 13929}}
[27195]  {:unknown, [13935]}
[27196]  {:push, {:value, 2543}}
[27198]  {:unknown, [2533]}
[27199]  {:push, {:value, 14078}}
[27201]  {:unknown, [14084]}
[27202]  {:push, {:value, 2548}}
[27204]  {:unknown, [2538]}
[27205]  {:pop, {:value, 14427}}
[27207]  {:unknown, [14433]}
[27208]  {:unknown, [14439]}
[27209]  {:pop, {:value, 2558}}
[27211]  {:unknown, [2543]}
[27212]  {:unknown, [2553]}
[27213]  {:set, {:value, 14629}, {:value, 1}}
[27216]  {:unknown, [2548]}
[27217]  {:push, {:value, 14753}}
[27219]  {:unknown, [14759]}
[27220]  {:push, {:value, 2623}}
[27222]  {:unknown, [2548]}
[27223]  {:push, {:value, 14905}}
[27225]  {:unknown, [14910]}
[27226]  {:push, {:value, 2568}}
[27228]  {:unknown, [2583]}
[27229]  {:pop, {:value, 15058}}
[27231]  {:unknown, [15063]}
[27232]  {:unknown, [15069]}
[27233]  {:pop, {:value, 2573}}
[27235]  {:unknown, [2588]}
[27236]  {:unknown, [2563]}
[27237]  {:pop, {:value, 15214}}
[27239]  {:unknown, [15219]}
[27240]  {:unknown, [15225]}
[27241]  {:pop, {:value, 2578}}
[27243]  {:unknown, [2593]}
[27244]  {:unknown, [2568]}
[27245]  {:pop, {:value, 15488}}
[27247]  {:unknown, [15494]}
[27248]  {:unknown, [15499]}
[27249]  {:pop, {:value, 2598}}
[27251]  {:unknown, [2573]}
[27252]  {:unknown, [2643]}
[27253]  {:pop, {:value, 15647}}
[27255]  {:unknown, [15653]}
[27256]  {:unknown, [15658]}
[27257]  {:pop, {:value, 2563}}
[27259]  {:unknown, [2588]}
[27260]  {:unknown, [2603]}
[27261]  {:eq, {:value, 15804}, {:value, 15810}, {:value, 15815}}
[27265]  {:unknown, [15821]}
[27266]  {:eq, {:value, 2568}, {:value, 2593}, {:value, 2608}}
[27270]  {:unknown, [2583]}
[27271]  {:eq, {:value, 15969}, {:value, 15975}, {:value, 15980}}
[27275]  {:unknown, [15986]}
[27276]  {:eq, {:value, 2573}, {:value, 2598}, {:value, 2613}}
[27280]  {:unknown, [2588]}
[27281]  {:pop, {:value, 16131}}
[27283]  {:unknown, [16137]}
[27284]  {:unknown, [16143]}
[27285]  {:pop, {:value, 2578}}
[27287]  {:unknown, [2618]}
[27288]  {:unknown, [2593]}
[27289]  {:pop, {:value, 16288}}
[27291]  {:unknown, [16294]}
[27292]  {:unknown, [16299]}
[27293]  {:pop, {:value, 2583}}
[27295]  {:unknown, [2608]}
[27296]  {:unknown, [2623]}
[27297]  {:eq, {:value, 16447}, {:value, 16453}, {:value, 16458}}
[27301]  {:unknown, [16464]}
[27302]  {:eq, {:value, 2588}, {:value, 2613}, {:value, 2628}}
[27306]  {:unknown, [2603]}
[27307]  {:eq, {:value, 16609}, {:value, 16615}, {:value, 16620}}
[27311]  {:unknown, [16626]}
[27312]  {:eq, {:value, 2593}, {:value, 2618}, {:value, 2633}}
[27316]  {:unknown, [2608]}
[27317]  {:pop, {:value, 16774}}
[27319]  {:unknown, [16780]}
[27320]  {:unknown, [16786]}
[27321]  {:pop, {:value, 2598}}
[27323]  {:unknown, [2638]}
[27324]  {:unknown, [2613]}
[27325]  {:pop, {:value, 16954}}
[27327]  {:unknown, [16960]}
[27328]  {:unknown, [16965]}
[27329]  {:pop, {:value, 2603}}
[27331]  {:unknown, [2628]}
[27332]  {:unknown, [2558]}
[27333]  {:pop, {:value, 17111}}
[27335]  {:unknown, [17117]}
[27336]  {:unknown, [17122]}
[27337]  {:pop, {:value, 2608}}
[27339]  {:unknown, [2633]}
[27340]  {:unknown, [2623]}
[27341]  {:pop, {:value, 17269}}
[27343]  {:unknown, [17275]}
[27344]  {:unknown, [17280]}
[27345]  {:pop, {:value, 2613}}
[27347]  {:unknown, [2638]}
[27348]  {:unknown, [2628]}
[27349]  {:push, {:value, 17425}}
[27351]  {:unknown, [17431]}
[27352]  {:push, {:value, 2618}}
[27354]  {:unknown, [2633]}
[27355]  {:set, {:value, 17655}, {:value, 1}}
[27358]  {:unknown, [2578]}
[27359]  {:push, {:value, 17793}}
[27361]  {:unknown, [17801]}
[27362]  {:push, {:value, 2653}}
[27364]  {:unknown, [2653]}
[27365]  {:push, {:value, 17933}}
[27367]  {:unknown, [17937]}
[27368]  {:push, {:value, 2658}}
[27370]  {:unknown, [2658]}
[27371]  {:pop, {:value, 18041}}
[27373]  {:unknown, [18045]}
[27374]  {:unknown, [18050]}
[27375]  {:pop, {:value, 2663}}
[27377]  {:unknown, [2663]}
[27378]  {:unknown, [2663]}
[27379]  {:halt}
[27380]  {:halt}
[27381]  {:wmem, {:value, 2668}, {:value, 2672}}				# Start of items table
[27384]  {:unknown, [2676]}
[27385]  {:unknown, [2680]}
[27386]  {:unknown, [2684]}
[27387]  {:unknown, [2688]}
[27388]  {:unknown, [2692]}
[27389]  {:unknown, [2696]}
[27390]  {:unknown, [2700]}
[27391]  {:unknown, [2704]}
[27392]  {:unknown, [2708]}
[27393]  {:unknown, [2712]}
[27394]  {:unknown, [2724]}
[27395]  {:unknown, [2728]}
[27396]  {:unknown, [2716]}
[27397]  {:unknown, [2720]}
[27398]  {:jt, {:value, 25943}, {:value, 25946}}
[27401]  {:unknown, [25951]}
[27402]  {:unknown, [25956]}
[27403]  {:unknown, [25960]}
[27404]  {:unknown, [25965]}
[27405]  {:unknown, [25970]}
[27406]  {:jt, {:value, 3245}, {:value, 2964}}
[27409]  {:unknown, [3333]}
[27410]  {:unknown, [3362]}
[27411]  {:unknown, [3400]}
[27412]  {:unknown, [3488]}
[27413]  {:unknown, [3568]}				# "Ptr to teleporter USE function"
[27414]  {:call, {:value, 17123}}
[27416]  {:unknown, [17086]}
[27417]  {:unknown, [17025]}
[27418]  {:unknown, [17032]}
[27419]  {:unknown, [17053]}
[27420]  {:unknown, [17097]}
[27421]  {:unknown, [17037]}
[27422]  {:unknown, [17030]}
[27423]  {:unknown, [17097]}
[27424]  {:unknown, [17040]}
[27425]  {:unknown, [17030]}
[27426]  {:unknown, [17052]}
[27427]  {:unknown, [17097]}
[27428]  {:unknown, [17037]}
[27429]  {:unknown, [17030]}
[27430]  {:unknown, [17110]}
[27431]  {:unknown, [17123]}
[27432]  {:unknown, [22]}
[27433]  {:unknown, [6714]}
[27434]  {:unknown, [6668]}
[27435]  {:unknown, [6678]}
[27436]  {:unknown, [6723]}
[27437]  {:unknown, [6672]}
[27438]  {:unknown, [6662]}
[27439]  {:unknown, [6662]}
[27440]  {:unknown, [6723]}
[27441]  {:unknown, [6669]}
[27442]  {:unknown, [6668]}
[27443]  {:unknown, [6723]}
[27444]  {:unknown, [6672]}
[27445]  {:unknown, [6678]}
[27446]  {:unknown, [6656]}
[27447]  {:unknown, [6667]}
[27448]  {:unknown, [6723]}
[27449]  {:unknown, [6666]}
[27450]  {:unknown, [6679]}
[27451]  {:unknown, [6662]}
[27452]  {:unknown, [6670]}
[27453]  {:unknown, [6733]}
[27454]  {:unknown, [6761]}
[27455]  {:unknown, [26]}
[27456]  {:unknown, [31614]}
[27457]  {:unknown, [31520]}
[27458]  {:unknown, [31516]}
[27459]  {:unknown, [31517]}
[27460]  {:unknown, [31514]}
[27461]  {:unknown, [31507]}
[27462]  {:unknown, [31495]}
[27463]  {:unknown, [31572]}
[27464]  {:unknown, [31515]}
[27465]  {:unknown, [31506]}
[27466]  {:unknown, [31572]}
[27467]  {:unknown, [31517]}
[27468]  {:unknown, [31514]}
[27469]  {:unknown, [31488]}
[27470]  {:unknown, [31505]}
[27471]  {:unknown, [31494]}
[27472]  {:unknown, [31505]}
[27473]  {:unknown, [31495]}
[27474]  {:unknown, [31488]}
[27475]  {:unknown, [31572]}
[27476]  {:unknown, [31516]}
[27477]  {:unknown, [31505]}
[27478]  {:unknown, [31494]}
[27479]  {:unknown, [31505]}
[27480]  {:unknown, [31566]}
[27481]  {:unknown, [31614]}
[27482]  {:unknown, '1'}
[27483]  {:unknown, [16945]}
[27484]  {:unknown, [16984]}
[27485]  {:unknown, [16924]}
[27486]  {:unknown, [16919]}
[27487]  {:unknown, [16918]}
[27488]  {:unknown, [16991]}
[27489]  {:unknown, [16908]}
[27490]  {:unknown, [16984]}
[27491]  {:unknown, [16909]}
[27492]  {:unknown, [16918]}
[27493]  {:unknown, [16924]}
[27494]  {:unknown, [16925]}
[27495]  {:unknown, [16906]}
[27496]  {:unknown, [16907]}
[27497]  {:unknown, [16908]}
[27498]  {:unknown, [16921]}
[27499]  {:unknown, [16918]}
[27500]  {:unknown, [16924]}
[27501]  {:unknown, [16963]}
[27502]  {:unknown, [16984]}
[27503]  {:unknown, [16908]}
[27504]  {:unknown, [16906]}
[27505]  {:unknown, [16897]}
[27506]  {:unknown, [16984]}
[27507]  {:unknown, [16991]}
[27508]  {:unknown, [16912]}
[27509]  {:unknown, [16925]}
[27510]  {:unknown, [16916]}
[27511]  {:unknown, [16904]}
[27512]  {:unknown, [16991]}
[27513]  {:unknown, [16984]}
[27514]  {:unknown, [16926]}
[27515]  {:unknown, [16919]}
[27516]  {:unknown, [16906]}
[27517]  {:unknown, [16984]}
[27518]  {:unknown, [16913]}
[27519]  {:unknown, [16918]}
[27520]  {:unknown, [16907]}
[27521]  {:unknown, [16908]}
[27522]  {:unknown, [16906]}
[27523]  {:unknown, [16909]}
[27524]  {:unknown, [16923]}
[27525]  {:unknown, [16908]}
[27526]  {:unknown, [16913]}
[27527]  {:unknown, [16919]}
[27528]  {:unknown, [16918]}
[27529]  {:unknown, [16907]}
[27530]  {:unknown, [16982]}
[27531]  {:unknown, [17010]}
[27532]  {:unknown, [504]}
[27533]  {:unknown, [30111]}
[27534]  {:unknown, [30108]}
[27535]  {:unknown, [30108]}
[27536]  {:unknown, [30104]}
[27537]  {:unknown, [30201]}
[27538]  {:unknown, [30163]}
[27539]  {:unknown, [30163]}
[27540]  {:unknown, [30122]}
[27541]  {:unknown, [30108]}
[27542]  {:unknown, [30086]}
[27543]  {:unknown, [30163]}
[27544]  {:unknown, [30110]}
[27545]  {:unknown, [30098]}
[27546]  {:unknown, [30090]}
[27547]  {:unknown, [30163]}
[27548]  {:unknown, [30110]}
[27549]  {:unknown, [30102]}
[27550]  {:unknown, [30081]}
[27551]  {:unknown, [30102]}
[27552]  {:unknown, [30111]}
[27553]  {:unknown, [30090]}
[27554]  {:unknown, [30163]}
[27555]  {:unknown, [30164]}
[27556]  {:unknown, [30111]}
[27557]  {:unknown, [30108]}
[27558]  {:unknown, [30108]}
[27559]  {:unknown, [30104]}
[27560]  {:unknown, [30164]}
[27561]  {:unknown, [30163]}
[27562]  {:unknown, [30087]}
[27563]  {:unknown, [30108]}
[27564]  {:unknown, [30163]}
[27565]  {:unknown, [30102]}
[27566]  {:unknown, [30091]}
[27567]  {:unknown, [30098]}
[27568]  {:unknown, [30110]}
[27569]  {:unknown, [30106]}
[27570]  {:unknown, [30109]}
[27571]  {:unknown, [30102]}
[27572]  {:unknown, [30163]}
[27573]  {:unknown, [30087]}
[27574]  {:unknown, [30107]}
[27575]  {:unknown, [30102]}
[27576]  {:unknown, [30163]}
[27577]  {:unknown, [30081]}
[27578]  {:unknown, [30108]}
[27579]  {:unknown, [30108]}
[27580]  {:unknown, [30110]}
[27581]  {:unknown, [30175]}
[27582]  {:unknown, [30163]}
[27583]  {:unknown, [30108]}
[27584]  {:unknown, [30081]}
[27585]  {:unknown, [30163]}
[27586]  {:unknown, [30090]}
[27587]  {:unknown, [30108]}
[27588]  {:unknown, [30086]}
[27589]  {:unknown, [30163]}
[27590]  {:unknown, [30110]}
[27591]  {:unknown, [30098]}
[27592]  {:unknown, [30090]}
[27593]  {:unknown, [30163]}
[27594]  {:unknown, [30164]}
[27595]  {:unknown, [30111]}
[27596]  {:unknown, [30108]}
[27597]  {:unknown, [30108]}
[27598]  {:unknown, [30104]}
[27599]  {:unknown, [30163]}
[27600]  {:unknown, [30159]}
[27601]  {:unknown, [30080]}
[27602]  {:unknown, [30086]}
[27603]  {:unknown, [30097]}
[27604]  {:unknown, [30105]}
[27605]  {:unknown, [30102]}
[27606]  {:unknown, [30096]}
[27607]  {:unknown, [30087]}
[27608]  {:unknown, [30157]}
[27609]  {:unknown, [30164]}
[27610]  {:unknown, [30163]}
[27611]  {:unknown, [30171]}
[27612]  {:unknown, [30080]}
[27613]  {:unknown, [30086]}
[27614]  {:unknown, [30096]}
[27615]  {:unknown, [30107]}
[27616]  {:unknown, [30163]}
[27617]  {:unknown, [30098]}
[27618]  {:unknown, [30080]}
[27619]  {:unknown, [30163]}
[27620]  {:unknown, [30164]}
[27621]  {:unknown, [30111]}
[27622]  {:unknown, [30108]}
[27623]  {:unknown, [30108]}
[27624]  {:unknown, [30104]}
[27625]  {:unknown, [30163]}
[27626]  {:unknown, [30096]}
[27627]  {:unknown, [30107]}
[27628]  {:unknown, [30098]}
[27629]  {:unknown, [30106]}
[27630]  {:unknown, [30081]}
[27631]  {:unknown, [30164]}
[27632]  {:unknown, [30170]}
[27633]  {:unknown, [30163]}
[27634]  {:unknown, [30087]}
[27635]  {:unknown, [30108]}
[27636]  {:unknown, [30163]}
[27637]  {:unknown, [30102]}
[27638]  {:unknown, [30091]}
[27639]  {:unknown, [30098]}
[27640]  {:unknown, [30110]}
[27641]  {:unknown, [30106]}
[27642]  {:unknown, [30109]}
[27643]  {:unknown, [30102]}
[27644]  {:unknown, [30163]}
[27645]  {:unknown, [30080]}
[27646]  {:unknown, [30108]}
[27647]  {:unknown, [30110]}
[27648]  {:unknown, [30102]}
[27649]  {:unknown, [30087]}
[27650]  {:unknown, [30107]}
[27651]  {:unknown, [30106]}
[27652]  {:unknown, [30109]}
[27653]  {:unknown, [30100]}
[27654]  {:unknown, [30163]}
[27655]  {:unknown, [30080]}
[27656]  {:unknown, [30083]}
[27657]  {:unknown, [30102]}
[27658]  {:unknown, [30096]}
[27659]  {:unknown, [30106]}
[27660]  {:unknown, [30101]}
[27661]  {:unknown, [30106]}
[27662]  {:unknown, [30096]}
[27663]  {:unknown, [30173]}
[27664]  {:unknown, [30201]}
[27665]  {:unknown, [30100]}
[27666]  {:unknown, [30108]}
[27667]  {:unknown, [30201]}
[27668]  {:unknown, [30163]}
[27669]  {:unknown, [30163]}
[27670]  {:unknown, [30122]}
[27671]  {:unknown, [30108]}
[27672]  {:unknown, [30086]}
[27673]  {:unknown, [30163]}
[27674]  {:unknown, [30110]}
[27675]  {:unknown, [30098]}
[27676]  {:unknown, [30090]}
[27677]  {:unknown, [30163]}
[27678]  {:unknown, [30164]}
[27679]  {:unknown, [30100]}
[27680]  {:unknown, [30108]}
[27681]  {:unknown, [30163]}
[27682]  {:unknown, [30159]}
[27683]  {:unknown, [30102]}
[27684]  {:unknown, [30091]}
[27685]  {:unknown, [30106]}
[27686]  {:unknown, [30087]}
[27687]  {:unknown, [30157]}
[27688]  {:unknown, [30164]}
[27689]  {:unknown, [30163]}
[27690]  {:unknown, [30087]}
[27691]  {:unknown, [30108]}
[27692]  {:unknown, [30163]}
[27693]  {:unknown, [30087]}
[27694]  {:unknown, [30081]}
[27695]  {:unknown, [30098]}
[27696]  {:unknown, [30085]}
[27697]  {:unknown, [30102]}
[27698]  {:unknown, [30111]}
[27699]  {:unknown, [30163]}
[27700]  {:unknown, [30106]}
[27701]  {:unknown, [30109]}
[27702]  {:unknown, [30163]}
[27703]  {:unknown, [30087]}
[27704]  {:unknown, [30107]}
[27705]  {:unknown, [30098]}
[27706]  {:unknown, [30087]}
[27707]  {:unknown, [30163]}
[27708]  {:unknown, [30103]}
[27709]  {:unknown, [30106]}
[27710]  {:unknown, [30081]}
[27711]  {:unknown, [30102]}
[27712]  {:unknown, [30096]}
[27713]  {:unknown, [30087]}
[27714]  {:unknown, [30106]}
[27715]  {:unknown, [30108]}
[27716]  {:unknown, [30109]}
[27717]  {:unknown, [30163]}
[27718]  {:unknown, [30171]}
[27719]  {:unknown, [30080]}
[27720]  {:unknown, [30086]}
[27721]  {:unknown, [30096]}
[27722]  {:unknown, [30107]}
[27723]  {:unknown, [30163]}
[27724]  {:unknown, [30098]}
[27725]  {:unknown, [30080]}
[27726]  {:unknown, [30163]}
[27727]  {:unknown, [30164]}
[27728]  {:unknown, [30100]}
[27729]  {:unknown, [30108]}
[27730]  {:unknown, [30163]}
[27731]  {:unknown, [30084]}
[27732]  {:unknown, [30102]}
[27733]  {:unknown, [30080]}
[27734]  {:unknown, [30087]}
[27735]  {:unknown, [30164]}
[27736]  {:unknown, [30170]}
[27737]  {:unknown, [30175]}
[27738]  {:unknown, [30163]}
[27739]  {:unknown, [30108]}
[27740]  {:unknown, [30081]}
[27741]  {:unknown, [30163]}
[27742]  {:unknown, [30090]}
[27743]  {:unknown, [30108]}
[27744]  {:unknown, [30086]}
[27745]  {:unknown, [30163]}
[27746]  {:unknown, [30110]}
[27747]  {:unknown, [30098]}
[27748]  {:unknown, [30090]}
[27749]  {:unknown, [30163]}
[27750]  {:unknown, [30110]}
[27751]  {:unknown, [30102]}
[27752]  {:unknown, [30081]}
[27753]  {:unknown, [30102]}
[27754]  {:unknown, [30111]}
[27755]  {:unknown, [30090]}
[27756]  {:unknown, [30163]}
[27757]  {:unknown, [30164]}
[27758]  {:unknown, [30159]}
[27759]  {:unknown, [30102]}
[27760]  {:unknown, [30091]}
[27761]  {:unknown, [30106]}
[27762]  {:unknown, [30087]}
[27763]  {:unknown, [30157]}
[27764]  {:unknown, [30164]}
[27765]  {:unknown, [30163]}
[27766]  {:unknown, [30171]}
[27767]  {:unknown, [30080]}
[27768]  {:unknown, [30086]}
[27769]  {:unknown, [30096]}
[27770]  {:unknown, [30107]}
[27771]  {:unknown, [30163]}
[27772]  {:unknown, [30098]}
[27773]  {:unknown, [30080]}
[27774]  {:unknown, [30163]}
[27775]  {:unknown, [30164]}
[27776]  {:unknown, [30084]}
[27777]  {:unknown, [30102]}
[27778]  {:unknown, [30080]}
[27779]  {:unknown, [30087]}
[27780]  {:unknown, [30164]}
[27781]  {:unknown, [30170]}
[27782]  {:unknown, [30173]}
[27783]  {:unknown, [30201]}
[27784]  {:unknown, [30106]}
[27785]  {:unknown, [30109]}
[27786]  {:unknown, [30085]}
[27787]  {:unknown, [30201]}
[27788]  {:unknown, [30163]}
[27789]  {:unknown, [30163]}
[27790]  {:unknown, [30119]}
[27791]  {:unknown, [30108]}
[27792]  {:unknown, [30163]}
[27793]  {:unknown, [30080]}
[27794]  {:unknown, [30102]}
[27795]  {:unknown, [30102]}
[27796]  {:unknown, [30163]}
[27797]  {:unknown, [30087]}
[27798]  {:unknown, [30107]}
[27799]  {:unknown, [30102]}
[27800]  {:unknown, [30163]}
[27801]  {:unknown, [30096]}
[27802]  {:unknown, [30108]}
[27803]  {:unknown, [30109]}
[27804]  {:unknown, [30087]}
[27805]  {:unknown, [30102]}
[27806]  {:unknown, [30109]}
[27807]  {:unknown, [30087]}
[27808]  {:unknown, [30080]}
[27809]  {:unknown, [30163]}
[27810]  {:unknown, [30108]}
[27811]  {:unknown, [30101]}
[27812]  {:unknown, [30163]}
[27813]  {:unknown, [30090]}
[27814]  {:unknown, [30108]}
[27815]  {:unknown, [30086]}
[27816]  {:unknown, [30081]}
[27817]  {:unknown, [30163]}
[27818]  {:unknown, [30106]}
[27819]  {:unknown, [30109]}
[27820]  {:unknown, [30085]}
[27821]  {:unknown, [30102]}
[27822]  {:unknown, [30109]}
[27823]  {:unknown, [30087]}
[27824]  {:unknown, [30108]}
[27825]  {:unknown, [30081]}
[27826]  {:unknown, [30090]}
[27827]  {:unknown, [30175]}
[27828]  {:unknown, [30163]}
[27829]  {:unknown, [30110]}
[27830]  {:unknown, [30102]}
[27831]  {:unknown, [30081]}
[27832]  {:unknown, [30102]}
[27833]  {:unknown, [30111]}
[27834]  {:unknown, [30090]}
[27835]  {:unknown, [30163]}
[27836]  {:unknown, [30164]}
[27837]  {:unknown, [30106]}
[27838]  {:unknown, [30109]}
[27839]  {:unknown, [30085]}
[27840]  {:unknown, [30164]}
[27841]  {:unknown, [30173]}
[27842]  {:unknown, [30201]}
[27843]  {:unknown, [30087]}
[27844]  {:unknown, [30098]}
[27845]  {:unknown, [30104]}
[27846]  {:unknown, [30102]}
[27847]  {:unknown, [30201]}
[27848]  {:unknown, [30163]}
[27849]  {:unknown, [30163]}
[27850]  {:unknown, [30122]}
[27851]  {:unknown, [30108]}
[27852]  {:unknown, [30086]}
[27853]  {:unknown, [30163]}
[27854]  {:unknown, [30110]}
[27855]  {:unknown, [30098]}
[27856]  {:unknown, [30090]}
[27857]  {:unknown, [30163]}
[27858]  {:unknown, [30164]}
[27859]  {:unknown, [30087]}
[27860]  {:unknown, [30098]}
[27861]  {:unknown, [30104]}
[27862]  {:unknown, [30102]}
[27863]  {:unknown, [30163]}
[27864]  {:unknown, [30159]}
[27865]  {:unknown, [30106]}
[27866]  {:unknown, [30087]}
[27867]  {:unknown, [30102]}
[27868]  {:unknown, [30110]}
[27869]  {:unknown, [30157]}
[27870]  {:unknown, [30164]}
[27871]  {:unknown, [30163]}
[27872]  {:unknown, [30171]}
[27873]  {:unknown, [30080]}
[27874]  {:unknown, [30086]}
[27875]  {:unknown, [30096]}
[27876]  {:unknown, [30107]}
[27877]  {:unknown, [30163]}
[27878]  {:unknown, [30098]}
[27879]  {:unknown, [30080]}
[27880]  {:unknown, [30163]}
[27881]  {:unknown, [30164]}
[27882]  {:unknown, [30087]}
[27883]  {:unknown, [30098]}
[27884]  {:unknown, [30104]}
[27885]  {:unknown, [30102]}
[27886]  {:unknown, [30163]}
[27887]  {:unknown, [30111]}
[27888]  {:unknown, [30098]}
[27889]  {:unknown, [30081]}
[27890]  {:unknown, [30100]}
[27891]  {:unknown, [30102]}
[27892]  {:unknown, [30163]}
[27893]  {:unknown, [30081]}
[27894]  {:unknown, [30108]}
[27895]  {:unknown, [30096]}
[27896]  {:unknown, [30104]}
[27897]  {:unknown, [30164]}
[27898]  {:unknown, [30170]}
[27899]  {:unknown, [30173]}
[27900]  {:unknown, [30201]}
[27901]  {:unknown, [30103]}
[27902]  {:unknown, [30081]}
[27903]  {:unknown, [30108]}
[27904]  {:unknown, [30083]}
[27905]  {:unknown, [30201]}
[27906]  {:unknown, [30163]}
[27907]  {:unknown, [30163]}
[27908]  {:unknown, [30119]}
[27909]  {:unknown, [30108]}
[27910]  {:unknown, [30163]}
[27911]  {:unknown, [30103]}
[27912]  {:unknown, [30081]}
[27913]  {:unknown, [30108]}
[27914]  {:unknown, [30083]}
[27915]  {:unknown, [30163]}
[27916]  {:unknown, [30080]}
[27917]  {:unknown, [30108]}
[27918]  {:unknown, [30110]}
[27919]  {:unknown, [30102]}
[27920]  {:unknown, [30087]}
[27921]  {:unknown, [30107]}
[27922]  {:unknown, [30106]}
[27923]  {:unknown, [30109]}
[27924]  {:unknown, [30100]}
[27925]  {:unknown, [30163]}
[27926]  {:unknown, [30106]}
[27927]  {:unknown, [30109]}
[27928]  {:unknown, [30163]}
[27929]  {:unknown, [30090]}
[27930]  {:unknown, [30108]}
[27931]  {:unknown, [30086]}
[27932]  {:unknown, [30081]}
[27933]  {:unknown, [30163]}
[27934]  {:unknown, [30106]}
[27935]  {:unknown, [30109]}
[27936]  {:unknown, [30085]}
[27937]  {:unknown, [30102]}
[27938]  {:unknown, [30109]}
[27939]  {:unknown, [30087]}
[27940]  {:unknown, [30108]}
[27941]  {:unknown, [30081]}
[27942]  {:unknown, [30090]}
[27943]  {:unknown, [30175]}
[27944]  {:unknown, [30163]}
[27945]  {:unknown, [30090]}
[27946]  {:unknown, [30108]}
[27947]  {:unknown, [30086]}
[27948]  {:unknown, [30163]}
[27949]  {:unknown, [30110]}
[27950]  {:unknown, [30098]}
[27951]  {:unknown, [30090]}
[27952]  {:unknown, [30163]}
[27953]  {:unknown, [30164]}
[27954]  {:unknown, [30103]}
[27955]  {:unknown, [30081]}
[27956]  {:unknown, [30108]}
[27957]  {:unknown, [30083]}
[27958]  {:unknown, [30163]}
[27959]  {:unknown, [30159]}
[27960]  {:unknown, [30106]}
[27961]  {:unknown, [30087]}
[27962]  {:unknown, [30102]}
[27963]  {:unknown, [30110]}
[27964]  {:unknown, [30157]}
[27965]  {:unknown, [30164]}
[27966]  {:unknown, [30173]}
[27967]  {:unknown, [30201]}
[27968]  {:unknown, [30086]}
[27969]  {:unknown, [30080]}
[27970]  {:unknown, [30102]}
[27971]  {:unknown, [30201]}
[27972]  {:unknown, [30163]}
[27973]  {:unknown, [30163]}
[27974]  {:unknown, [30122]}
[27975]  {:unknown, [30108]}
[27976]  {:unknown, [30086]}
[27977]  {:unknown, [30163]}
[27978]  {:unknown, [30110]}
[27979]  {:unknown, [30098]}
[27980]  {:unknown, [30090]}
[27981]  {:unknown, [30163]}
[27982]  {:unknown, [30098]}
[27983]  {:unknown, [30096]}
[27984]  {:unknown, [30087]}
[27985]  {:unknown, [30106]}
[27986]  {:unknown, [30085]}
[27987]  {:unknown, [30098]}
[27988]  {:unknown, [30087]}
[27989]  {:unknown, [30102]}
[27990]  {:unknown, [30163]}
[27991]  {:unknown, [30108]}
[27992]  {:unknown, [30081]}
[27993]  {:unknown, [30163]}
[27994]  {:unknown, [30108]}
[27995]  {:unknown, [30087]}
[27996]  {:unknown, [30107]}
[27997]  {:unknown, [30102]}
[27998]  {:unknown, [30081]}
[27999]  {:unknown, [30084]}
[28000]  {:unknown, [30106]}
[28001]  {:unknown, [30080]}
[28002]  {:unknown, [30102]}
[28003]  {:unknown, [30163]}
[28004]  {:unknown, [30098]}
[28005]  {:unknown, [30083]}
[28006]  {:unknown, [30083]}
[28007]  {:unknown, [30111]}
[28008]  {:unknown, [30090]}
[28009]  {:unknown, [30163]}
[28010]  {:unknown, [30098]}
[28011]  {:unknown, [30109]}
[28012]  {:unknown, [30163]}
[28013]  {:unknown, [30106]}
[28014]  {:unknown, [30087]}
[28015]  {:unknown, [30102]}
[28016]  {:unknown, [30110]}
[28017]  {:unknown, [30163]}
[28018]  {:unknown, [30084]}
[28019]  {:unknown, [30106]}
[28020]  {:unknown, [30087]}
[28021]  {:unknown, [30107]}
[28022]  {:unknown, [30163]}
[28023]  {:unknown, [30164]}
[28024]  {:unknown, [30086]}
[28025]  {:unknown, [30080]}
[28026]  {:unknown, [30102]}
[28027]  {:unknown, [30163]}
[28028]  {:unknown, [30159]}
[28029]  {:unknown, [30106]}
[28030]  {:unknown, [30087]}
[28031]  {:unknown, [30102]}
[28032]  {:unknown, [30110]}
[28033]  {:unknown, [30157]}
[28034]  {:unknown, [30164]}
[28035]  {:unknown, [30173]}
[28036]  {:unknown, [30201]}
[28037]  {:wmem, {:value, 1829}, {:value, 1811}}
[28040]  {:unknown, [1801]}
[28041]  {:unknown, [1806]}
[28042]  {:unknown, [1884]}
[28043]  {:unknown, [1813]}
[28044]  {:unknown, [1810]}
[28045]  {:unknown, [1802]}
[28046]  {:unknown, [1817]}
[28047]  {:unknown, [1810]}
[28048]  {:unknown, [1800]}
[28049]  {:unknown, [1811]}
[28050]  {:unknown, [1806]}
[28051]  {:unknown, [1797]}
[28052]  {:unknown, [1862]}
[28053]  {:unknown, [1910]}
[28054]  {:jt, {:value, 6079}, {:value, 6026}}
[28057]  {:unknown, [6016]}
[28058]  {:unknown, [6030]}
[28059]  {:unknown, [6021]}
[28060]  {:unknown, [6085]}
[28061]  {:unknown, [6113]}
[28062]  {:unknown, '\e'}
[28063]  {:unknown, [23338]}
[28064]  {:unknown, [23324]}
[28065]  {:unknown, [23302]}
[28066]  {:unknown, [23379]}
[28067]  {:unknown, [23296]}
[28068]  {:unknown, [23318]}
[28069]  {:unknown, [23318]}
[28070]  {:unknown, [23379]}
[28071]  {:unknown, [23325]}
[28072]  {:unknown, [23324]}
[28073]  {:unknown, [23379]}
[28074]  {:unknown, [23296]}
[28075]  {:unknown, [23302]}
[28076]  {:unknown, [23312]}
[28077]  {:unknown, [23323]}
[28078]  {:unknown, [23379]}
[28079]  {:unknown, [23322]}
[28080]  {:unknown, [23303]}
[28081]  {:unknown, [23318]}
[28082]  {:unknown, [23326]}
[28083]  {:unknown, [23379]}
[28084]  {:unknown, [23323]}
[28085]  {:unknown, [23318]}
[28086]  {:unknown, [23297]}
[28087]  {:unknown, [23318]}
[28088]  {:unknown, [23389]}
[28089]  {:unknown, [23417]}
[28090]  {:add, {:value, 15913}, {:value, 15903}, {:value, 15874}}
[28094]  {:unknown, [15901]}
[28095]  {:unknown, [15901]}
[28096]  {:unknown, [15880]}
[28097]  {:unknown, [15881]}
[28098]  {:unknown, [15939]}
[28099]  {:unknown, [15975]}
[28100]  {:unknown, '"'}
[28101]  {:unknown, [32682]}
[28102]  {:unknown, [32668]}
[28103]  {:unknown, [32646]}
[28104]  {:unknown, [32723]}
[28105]  {:unknown, [32656]}
[28106]  {:unknown, [32658]}
[28107]  {:unknown, [32669]}
[28108]  {:unknown, [32724]}
[28109]  {:unknown, [32647]}
[28110]  {:unknown, [32723]}
[28111]  {:unknown, [32661]}
[28112]  {:unknown, [32666]}
[28113]  {:unknown, [32669]}
[28114]  {:unknown, [32663]}
[28115]  {:unknown, [32723]}
[28116]  {:unknown, [32647]}
[28117]  {:unknown, [32667]}
[28118]  {:unknown, [32658]}
[28119]  {:unknown, [32647]}
[28120]  {:unknown, [32723]}
[28121]  {:unknown, [32666]}
[28122]  {:unknown, [32669]}
[28123]  {:unknown, [32723]}
[28124]  {:unknown, [32650]}
[28125]  {:unknown, [32668]}
[28126]  {:unknown, [32646]}
[28127]  {:unknown, [32641]}
[28128]  {:unknown, [32723]}
[28129]  {:unknown, [32643]}
[28130]  {:unknown, [32658]}
[28131]  {:unknown, [32656]}
[28132]  {:unknown, [32664]}
[28133]  {:unknown, [32733]}
[28134]  {:unknown, [32761]}
[28135]  {:unknown, '"'}
[28136]  {:unknown, [28219]}
[28137]  {:unknown, [28173]}
[28138]  {:unknown, [28183]}
[28139]  {:unknown, [28226]}
[28140]  {:unknown, [28161]}
[28141]  {:unknown, [28163]}
[28142]  {:unknown, [28172]}
[28143]  {:unknown, [28229]}
[28144]  {:unknown, [28182]}
[28145]  {:unknown, [28226]}
[28146]  {:unknown, [28164]}
[28147]  {:unknown, [28171]}
[28148]  {:unknown, [28172]}
[28149]  {:unknown, [28166]}
[28150]  {:unknown, [28226]}
[28151]  {:unknown, [28182]}
[28152]  {:unknown, [28170]}
[28153]  {:unknown, [28163]}
[28154]  {:unknown, [28182]}
[28155]  {:unknown, [28226]}
[28156]  {:unknown, [28171]}
[28157]  {:unknown, [28172]}
[28158]  {:unknown, [28226]}
[28159]  {:unknown, [28187]}
[28160]  {:unknown, [28173]}
[28161]  {:unknown, [28183]}
[28162]  {:unknown, [28176]}
[28163]  {:unknown, [28226]}
[28164]  {:unknown, [28178]}
[28165]  {:unknown, [28163]}
[28166]  {:unknown, [28161]}
[28167]  {:unknown, [28169]}
[28168]  {:unknown, [28236]}
[28169]  {:unknown, [28264]}
[28170]  {:unknown, '!'}
[28171]  {:unknown, [6718]}
[28172]  {:unknown, [6664]}
[28173]  {:unknown, [6674]}
[28174]  {:unknown, [6727]}
[28175]  {:unknown, [6662]}
[28176]  {:unknown, [6677]}
[28177]  {:unknown, [6658]}
[28178]  {:unknown, [6665]}
[28179]  {:unknown, [6720]}
[28180]  {:unknown, [6675]}
[28181]  {:unknown, [6727]}
[28182]  {:unknown, [6676]}
[28183]  {:unknown, [6674]}
[28184]  {:unknown, [6677]}
[28185]  {:unknown, [6658]}
[28186]  {:unknown, [6727]}
[28187]  {:unknown, [6671]}
[28188]  {:unknown, [6664]}
[28189]  {:unknown, [6672]}
[28190]  {:unknown, [6727]}
[28191]  {:unknown, [6675]}
[28192]  {:unknown, [6664]}
[28193]  {:unknown, [6727]}
[28194]  {:unknown, [6674]}
[28195]  {:unknown, [6676]}
[28196]  {:unknown, [6658]}
[28197]  {:unknown, [6727]}
[28198]  {:unknown, [6675]}
[28199]  {:unknown, [6671]}
[28200]  {:unknown, [6662]}
[28201]  {:unknown, [6675]}
[28202]  {:unknown, [6729]}
[28203]  {:unknown, [6765]}
[28204]  {:unknown, [31]}
[28205]  {:unknown, [13754]}
[28206]  {:unknown, [13708]}
[28207]  {:unknown, [13718]}
[28208]  {:unknown, [13763]}
[28209]  {:unknown, [13707]}
[28210]  {:unknown, [13698]}
[28211]  {:unknown, [13717]}
[28212]  {:unknown, [13702]}
[28213]  {:unknown, [13763]}
[28214]  {:unknown, [13697]}
[28215]  {:unknown, [13702]}
[28216]  {:unknown, [13702]}
[28217]  {:unknown, [13709]}
[28218]  {:unknown, [13763]}
[28219]  {:unknown, [13702]}
[28220]  {:unknown, [13698]}
[28221]  {:unknown, [13719]}
[28222]  {:unknown, [13702]}
[28223]  {:unknown, [13709]}
[28224]  {:unknown, [13763]}
[28225]  {:unknown, [13697]}
[28226]  {:unknown, [13722]}
[28227]  {:unknown, [13763]}
[28228]  {:unknown, [13698]}
[28229]  {:unknown, [13763]}
[28230]  {:unknown, [13700]}
[28231]  {:unknown, [13713]}
[28232]  {:unknown, [13718]}
[28233]  {:unknown, [13702]}
[28234]  {:unknown, [13773]}
[28235]  {:unknown, [13801]}
[28236]  {:unknown, '>'}
[28237]  {:unknown, [7476]}
[28238]  {:unknown, [7455]}
[28239]  {:unknown, [7454]}
[28240]  {:unknown, [7428]}
[28241]  {:unknown, [7442]}
[28242]  {:unknown, [7451]}
[28243]  {:unknown, [7442]}
[28244]  {:unknown, [7443]}
[28245]  {:unknown, [7511]}
[28246]  {:unknown, [7448]}
[28247]  {:unknown, [7449]}
[28248]  {:unknown, [7511]}
[28249]  {:unknown, [7427]}
[28250]  {:unknown, [7455]}
[28251]  {:unknown, [7442]}
[28252]  {:unknown, [7511]}
[28253]  {:unknown, [7424]}
[28254]  {:unknown, [7446]}
[28255]  {:unknown, [7451]}
[28256]  {:unknown, [7451]}
[28257]  {:unknown, [7511]}
[28258]  {:unknown, [7448]}
[28259]  {:unknown, [7441]}
[28260]  {:unknown, [7511]}
[28261]  {:unknown, [7448]}
[28262]  {:unknown, [7449]}
[28263]  {:unknown, [7442]}
[28264]  {:unknown, [7511]}
[28265]  {:unknown, [7448]}
[28266]  {:unknown, [7441]}
[28267]  {:unknown, [7511]}
[28268]  {:unknown, [7427]}
[28269]  {:unknown, [7455]}
[28270]  {:unknown, [7442]}
[28271]  {:unknown, [7511]}
[28272]  {:unknown, [7431]}
[28273]  {:unknown, [7446]}
[28274]  {:unknown, [7428]}
[28275]  {:unknown, [7428]}
[28276]  {:unknown, [7446]}
[28277]  {:unknown, [7440]}
[28278]  {:unknown, [7442]}
[28279]  {:unknown, [7424]}
[28280]  {:unknown, [7446]}
[28281]  {:unknown, [7438]}
[28282]  {:unknown, [7428]}
[28283]  {:unknown, [7515]}
[28284]  {:unknown, [7511]}
[28285]  {:unknown, [7438]}
[28286]  {:unknown, [7448]}
[28287]  {:unknown, [7426]}
[28288]  {:unknown, [7511]}
[28289]  {:unknown, [7428]}
[28290]  {:unknown, [7442]}
[28291]  {:unknown, [7442]}
[28292]  {:unknown, [7501]}
[28293]  {:unknown, [7549]}
[28294]  {:unknown, [7549]}
[28295]  {:unknown, [7511]}
[28296]  {:unknown, [7511]}
[28297]  {:unknown, [7511]}
[28298]  {:unknown, [7511]}
[28299]  {:pop, {:value, 1866}}
[28301]  {:unknown, [18824]}
[28302]  {:unknown, [26158]}
[28303]  {:unknown, '+'}
[28304]  {:unknown, [24702]}
[28305]  {:unknown, [24702]}
[28306]  {:unknown, [24621]}
[28307]  {:unknown, [24603]}
[28308]  {:unknown, [24577]}
[28309]  {:unknown, [24660]}
[28310]  {:unknown, [24576]}
[28311]  {:unknown, [24597]}
[28312]  {:unknown, [24607]}
[28313]  {:unknown, [24593]}
[28314]  {:unknown, [24660]}
[28315]  {:unknown, [24602]}
[28316]  {:unknown, [24603]}
[28317]  {:unknown, [24576]}
[28318]  {:unknown, [24593]}
[28319]  {:unknown, [24660]}
[28320]  {:unknown, [24603]}
[28321]  {:unknown, [24594]}
[28322]  {:unknown, [24660]}
[28323]  {:unknown, [24576]}
[28324]  {:unknown, [24604]}
[28325]  {:unknown, [24605]}
[28326]  {:unknown, [24583]}
[28327]  {:unknown, [24660]}
[28328]  {:unknown, [24597]}
[28329]  {:unknown, [24602]}
[28330]  {:unknown, [24592]}
[28331]  {:unknown, [24660]}
[28332]  {:unknown, [24607]}
[28333]  {:unknown, [24593]}
[28334]  {:unknown, [24593]}
[28335]  {:unknown, [24580]}
[28336]  {:unknown, [24660]}
[28337]  {:unknown, [24579]}
[28338]  {:unknown, [24597]}
[28339]  {:unknown, [24600]}
[28340]  {:unknown, [24607]}
[28341]  {:unknown, [24605]}
[28342]  {:unknown, [24602]}
[28343]  {:unknown, [24595]}
[28344]  {:unknown, [24666]}
[28345]  {:unknown, [24702]}
[28346]  {:unknown, [24702]}
[28347]  {:noop}
[28348]  {:unknown, [548]}
[28349]  {:unknown, [536]}
[28350]  {:unknown, [529]}
[28351]  {:unknown, [516]}
[28352]  {:unknown, [592]}
[28353]  {:unknown, [532]}
[28354]  {:unknown, [543]}
[28355]  {:unknown, [543]}
[28356]  {:unknown, [514]}
[28357]  {:unknown, [592]}
[28358]  {:unknown, [537]}
[28359]  {:unknown, [515]}
[28360]  {:unknown, [592]}
[28361]  {:unknown, [540]}
[28362]  {:unknown, [543]}
[28363]  {:unknown, [531]}
[28364]  {:unknown, [539]}
[28365]  {:unknown, [533]}
[28366]  {:unknown, [532]}
[28367]  {:unknown, [606]}
[28368]  {:unknown, [634]}
[28369]  {:unknown, '\e'}
[28370]  {:unknown, [22704]}
[28371]  {:unknown, [22662]}
[28372]  {:unknown, [22684]}
[28373]  {:unknown, [22729]}
[28374]  {:unknown, [22671]}
[28375]  {:unknown, [22656]}
[28376]  {:unknown, [22663]}
[28377]  {:unknown, [22669]}
[28378]  {:unknown, [22729]}
[28379]  {:unknown, [22672]}
[28380]  {:unknown, [22662]}
[28381]  {:unknown, [22684]}
[28382]  {:unknown, [22683]}
[28383]  {:unknown, [22682]}
[28384]  {:unknown, [22668]}
[28385]  {:unknown, [22661]}
[28386]  {:unknown, [22671]}
[28387]  {:unknown, [22729]}
[28388]  {:unknown, [22686]}
[28389]  {:unknown, [22683]}
[28390]  {:unknown, [22656]}
[28391]  {:unknown, [22685]}
[28392]  {:unknown, [22656]}
[28393]  {:unknown, [22663]}
[28394]  {:unknown, [22670]}
[28395]  {:unknown, [22729]}
[28396]  {:unknown, [22731]}
[28397]  {:pop, {:value, 14100}}
[28399]  {:unknown, [11224]}
[28400]  {:unknown, [13004]}
[28401]  {:unknown, '3'}
[28402]  {:unknown, [25436]}
[28403]  {:unknown, [25438]}
[28404]  {:unknown, [25361]}
[28405]  {:unknown, [25360]}
[28406]  {:unknown, [25438]}
[28407]  {:unknown, [25354]}
[28408]  {:unknown, [25366]}
[28409]  {:unknown, [25371]}
[28410]  {:unknown, [25438]}
[28411]  {:unknown, [25354]}
[28412]  {:unknown, [25375]}
[28413]  {:unknown, [25372]}
[28414]  {:unknown, [25362]}
[28415]  {:unknown, [25371]}
[28416]  {:unknown, [25354]}
[28417]  {:unknown, [25424]}
[28418]  {:unknown, [25438]}
[28419]  {:unknown, [25438]}
[28420]  {:unknown, [25390]}
[28421]  {:unknown, [25371]}
[28422]  {:unknown, [25356]}
[28423]  {:unknown, [25366]}
[28424]  {:unknown, [25375]}
[28425]  {:unknown, [25358]}
[28426]  {:unknown, [25357]}
[28427]  {:unknown, [25438]}
[28428]  {:unknown, [25367]}
[28429]  {:unknown, [25354]}
[28430]  {:unknown, [25433]}
[28431]  {:unknown, [25357]}
[28432]  {:unknown, [25438]}
[28433]  {:unknown, [25357]}
[28434]  {:unknown, [25361]}
[28435]  {:unknown, [25363]}
[28436]  {:unknown, [25371]}
[28437]  {:unknown, [25438]}
[28438]  {:unknown, [25365]}
[28439]  {:unknown, [25367]}
[28440]  {:unknown, [25360]}
[28441]  {:unknown, [25370]}
[28442]  {:unknown, [25438]}
[28443]  {:unknown, [25361]}
[28444]  {:unknown, [25368]}
[28445]  {:unknown, [25438]}
[28446]  {:unknown, [25373]}
[28447]  {:unknown, [25361]}
[28448]  {:unknown, [25370]}
[28449]  {:unknown, [25371]}
[28450]  {:unknown, [25409]}
[28451]  {:unknown, [25460]}
[28452]  {:unknown, [25460]}
[28453]  {:unknown, '8'}
[28454]  {:unknown, [16952]}
[28455]  {:unknown, [16910]}
[28456]  {:unknown, [16916]}
[28457]  {:unknown, [16961]}
[28458]  {:unknown, [16903]}
[28459]  {:unknown, [16904]}
[28460]  {:unknown, [16909]}
[28461]  {:unknown, [16909]}
[28462]  {:unknown, [16961]}
[28463]  {:unknown, [16920]}
[28464]  {:unknown, [16910]}
[28465]  {:unknown, [16916]}
[28466]  {:unknown, [16915]}
[28467]  {:unknown, [16961]}
[28468]  {:unknown, [16909]}
[28469]  {:unknown, [16896]}
[28470]  {:unknown, [16911]}
[28471]  {:unknown, [16917]}
[28472]  {:unknown, [16900]}
[28473]  {:unknown, [16915]}
[28474]  {:unknown, [16911]}
[28475]  {:unknown, [16961]}
[28476]  {:unknown, [16918]}
[28477]  {:unknown, [16904]}
[28478]  {:unknown, [16917]}
[28479]  {:unknown, [16905]}
[28480]  {:unknown, [16961]}
[28481]  {:unknown, [16910]}
[28482]  {:unknown, [16904]}
[28483]  {:unknown, [16909]}
[28484]  {:unknown, [16975]}
[28485]  {:unknown, [16961]}
[28486]  {:unknown, [16961]}
[28487]  {:unknown, [16936]}
[28488]  {:unknown, [16917]}
[28489]  {:unknown, [16961]}
[28490]  {:unknown, [16914]}
[28491]  {:unknown, [16900]}
[28492]  {:unknown, [16900]}
[28493]  {:unknown, [16908]}
[28494]  {:unknown, [16914]}
[28495]  {:unknown, [16961]}
[28496]  {:unknown, [16917]}
[28497]  {:unknown, [16910]}
[28498]  {:unknown, [16961]}
[28499]  {:unknown, [16898]}
[28500]  {:unknown, [16905]}
[28501]  {:unknown, [16900]}
[28502]  {:unknown, [16900]}
[28503]  {:unknown, [16915]}
[28504]  {:unknown, [16961]}
[28505]  {:unknown, [16916]}
[28506]  {:unknown, [16913]}
[28507]  {:unknown, [16960]}
[28508]  {:unknown, [17003]}
[28509]  {:unknown, [17003]}
[28510]  {:unknown, ':'}
[28511]  {:unknown, '&'}
[28512]  {:wmem, {:value, 10}, {:value, 88}}
[28515]  {:out, {:value, [19]}}
[28517]  {:unknown, '_'}
[28518]  {:unknown, [23]}
[28519]  {:unknown, [30]}
[28520]  {:add, {:value, 26}, {:value, 95}, {:value, 11}}
[28524]  {:wmem, {:value, 95}, {:value, 25}}
[28527]  {:unknown, [22]}
[28528]  {:call, {:value, 27}}
[28530]  {:unknown, '_'}
[28531]  {:and, {:value, 16}, {:value, 18}, {:value, 26}}
[28535]  {:mod, {:value, 23}, {:value, 22}, {:value, 17}}
[28539]  {:unknown, [24]}
[28540]  {:unknown, '_'}
[28541]  {:mod, {:value, 16}, {:value, 95}, {:value, 15}}
[28545]  {:mult, {:value, 11}, {:value, 95}, {:value, 11}}
[28549]  {:unknown, [23]}
[28550]  {:unknown, [26]}
[28551]  {:unknown, '_'}
[28552]  {:wmem, {:value, 22}, {:value, 19}}
[28555]  {:unknown, '_'}
[28556]  {:unknown, [22]}
[28557]  {:call, {:value, 11}}
[28559]  {:wmem, {:value, 95}, {:value, 25}}
[28562]  {:unknown, [22]}
[28563]  {:or, {:value, 12}, {:value, 11}, {:value, 81}}
[28567]  {:unknown, 'u'}
[28568]  {:unknown, 'u'}
[28569]  {:unknown, [25]}
[28570]  {:unknown, [959]}
[28571]  {:unknown, [905]}
[28572]  {:unknown, [915]}
[28573]  {:unknown, [966]}
[28574]  {:unknown, [906]}
[28575]  {:unknown, [911]}
[28576]  {:unknown, [897]}
[28577]  {:unknown, [910]}
[28578]  {:unknown, [914]}
[28579]  {:unknown, [966]}
[28580]  {:unknown, [927]}
[28581]  {:unknown, [905]}
[28582]  {:unknown, [915]}
[28583]  {:unknown, [916]}
[28584]  {:unknown, [966]}
[28585]  {:unknown, [906]}
[28586]  {:unknown, [903]}
[28587]  {:unknown, [904]}
[28588]  {:unknown, [914]}
[28589]  {:unknown, [899]}
[28590]  {:unknown, [916]}
[28591]  {:unknown, [904]}
[28592]  {:unknown, [968]}
[28593]  {:unknown, [1004]}
[28594]  {:unknown, [1004]}
[28595]  {:unknown, [25]}
[28596]  {:unknown, [10420]}
[28597]  {:unknown, [10370]}
[28598]  {:unknown, [10392]}
[28599]  {:unknown, [10445]}
[28600]  {:unknown, [10377]}
[28601]  {:unknown, [10370]}
[28602]  {:unknown, [10392]}
[28603]  {:unknown, [10398]}
[28604]  {:unknown, [10376]}
[28605]  {:unknown, [10445]}
[28606]  {:unknown, [10388]}
[28607]  {:unknown, [10370]}
[28608]  {:unknown, [10392]}
[28609]  {:unknown, [10399]}
[28610]  {:unknown, [10445]}
[28611]  {:unknown, [10369]}
[28612]  {:unknown, [10380]}
[28613]  {:unknown, [10371]}
[28614]  {:unknown, [10393]}
[28615]  {:unknown, [10376]}
[28616]  {:unknown, [10399]}
[28617]  {:unknown, [10371]}
[28618]  {:unknown, [10435]}
[28619]  {:unknown, [10471]}
[28620]  {:unknown, [10471]}
[28621]  {:unknown, '*'}
[28622]  {:unknown, [27825]}
[28623]  {:unknown, [27783]}
[28624]  {:unknown, [27805]}
[28625]  {:unknown, [27855]}
[28626]  {:unknown, [27802]}
[28627]  {:unknown, [27789]}
[28628]  {:unknown, [27848]}
[28629]  {:unknown, [27782]}
[28630]  {:unknown, [27783]}
[28631]  {:unknown, [27804]}
[28632]  {:unknown, [27848]}
[28633]  {:unknown, [27803]}
[28634]  {:unknown, [27805]}
[28635]  {:unknown, [27802]}
[28636]  {:unknown, [27789]}
[28637]  {:unknown, [27848]}
[28638]  {:unknown, [27807]}
[28639]  {:unknown, [27776]}
[28640]  {:unknown, [27785]}
[28641]  {:unknown, [27804]}
[28642]  {:unknown, [27848]}
[28643]  {:unknown, [27804]}
[28644]  {:unknown, [27783]}
[28645]  {:unknown, [27848]}
[28646]  {:unknown, [27788]}
[28647]  {:unknown, [27783]}
[28648]  {:unknown, [27848]}
[28649]  {:unknown, [27807]}
[28650]  {:unknown, [27777]}
[28651]  {:unknown, [27804]}
[28652]  {:unknown, [27776]}
[28653]  {:unknown, [27848]}
[28654]  {:unknown, [27804]}
[28655]  {:unknown, [27776]}
[28656]  {:unknown, [27789]}
[28657]  {:unknown, [27848]}
[28658]  {:unknown, [27787]}
[28659]  {:unknown, [27783]}
[28660]  {:unknown, [27777]}
[28661]  {:unknown, [27782]}
[28662]  {:unknown, [27846]}
[28663]  {:unknown, [27874]}
[28664]  {:not, {:value, 13361}, {:value, 13319}}
[28667]  {:unknown, [13341]}
[28668]  {:unknown, [13384]}
[28669]  {:unknown, [13336]}
[28670]  {:unknown, [13316]}
[28671]  {:unknown, [13321]}
[28672]  {:unknown, [13323]}
[28673]  {:unknown, [13325]}
[28674]  {:unknown, [13384]}
[28675]  {:unknown, [13340]}
[28676]  {:unknown, [13312]}
[28677]  {:unknown, [13325]}
[28678]  {:unknown, [13384]}
[28679]  {:unknown, [30]}
[28680]  {:unknown, [10566]}
[28681]  {:unknown, [10511]}
[28682]  {:unknown, [10504]}
[28683]  {:unknown, [10514]}
[28684]  {:unknown, [10505]}
[28685]  {:unknown, [10566]}
[28686]  {:unknown, [10514]}
[28687]  {:unknown, [10510]}
[28688]  {:unknown, [10499]}
[28689]  {:unknown, [10566]}
[28690]  {:unknown, [10506]}
[28691]  {:unknown, [10499]}
[28692]  {:unknown, [10496]}
[28693]  {:unknown, [10514]}
[28694]  {:unknown, [10507]}
[28695]  {:unknown, [10505]}
[28696]  {:unknown, [10517]}
[28697]  {:unknown, [10514]}
[28698]  {:unknown, [10566]}
[28699]  {:unknown, [10505]}
[28700]  {:unknown, [10518]}
[28701]  {:unknown, [10499]}
[28702]  {:unknown, [10504]}
[28703]  {:unknown, [10566]}
[28704]  {:unknown, [10517]}
[28705]  {:unknown, [10506]}
[28706]  {:unknown, [10505]}
[28707]  {:unknown, [10514]}
[28708]  {:unknown, [10568]}
[28709]  {:unknown, [10604]}
[28710]  {:unknown, 'B'}
[28711]  {:unknown, [29501]}
[28712]  {:unknown, [29455]}
[28713]  {:unknown, [29532]}
[28714]  {:unknown, [29445]}
[28715]  {:unknown, [29459]}
[28716]  {:unknown, [29449]}
[28717]  {:unknown, [29532]}
[28718]  {:unknown, [29452]}
[28719]  {:unknown, [29456]}
[28720]  {:unknown, [29469]}
[28721]  {:unknown, [29471]}
[28722]  {:unknown, [29465]}
[28723]  {:unknown, [29532]}
[28724]  {:unknown, [29448]}
[28725]  {:unknown, [29460]}
[28726]  {:unknown, [29465]}
[28727]  {:unknown, [29532]}
[28728]  {:unknown, [29456]}
[28729]  {:unknown, [29469]}
[28730]  {:unknown, [29455]}
[28731]  {:unknown, [29448]}
[28732]  {:unknown, [29532]}
[28733]  {:unknown, [29471]}
[28734]  {:unknown, [29459]}
[28735]  {:unknown, [29461]}
[28736]  {:unknown, [29458]}
[28737]  {:unknown, [29520]}
[28738]  {:unknown, [29532]}
[28739]  {:unknown, [29448]}
[28740]  {:unknown, [29460]}
[28741]  {:unknown, [29465]}
[28742]  {:unknown, [29445]}
[28743]  {:unknown, [29532]}
[28744]  {:unknown, [29469]}
[28745]  {:unknown, [29454]}
[28746]  {:unknown, [29465]}
[28747]  {:unknown, [29532]}
[28748]  {:unknown, [29469]}
[28749]  {:unknown, [29456]}
[28750]  {:unknown, [29456]}
[28751]  {:unknown, [29532]}
[28752]  {:unknown, [29454]}
[28753]  {:unknown, [29465]}
[28754]  {:unknown, [29456]}
[28755]  {:unknown, [29465]}
[28756]  {:unknown, [29469]}
[28757]  {:unknown, [29455]}
[28758]  {:unknown, [29465]}
[28759]  {:unknown, [29464]}
[28760]  {:unknown, [29532]}
[28761]  {:unknown, [29459]}
[28762]  {:unknown, [29458]}
[28763]  {:unknown, [29448]}
[28764]  {:unknown, [29459]}
[28765]  {:unknown, [29532]}
[28766]  {:unknown, [29448]}
[28767]  {:unknown, [29460]}
[28768]  {:unknown, [29465]}
[28769]  {:unknown, [29532]}
[28770]  {:unknown, [29466]}
[28771]  {:unknown, [29456]}
[28772]  {:unknown, [29459]}
[28773]  {:unknown, [29459]}
[28774]  {:unknown, [29454]}
[28775]  {:unknown, [29522]}
[28776]  {:unknown, [29558]}
[28777]  {:unknown, 'B'}
[28778]  {:unknown, [17967]}
[28779]  {:unknown, [17949]}
[28780]  {:unknown, [17998]}
[28781]  {:unknown, [17943]}
[28782]  {:unknown, [17921]}
[28783]  {:unknown, [17947]}
[28784]  {:unknown, [17998]}
[28785]  {:unknown, [17950]}
[28786]  {:unknown, [17922]}
[28787]  {:unknown, [17935]}
[28788]  {:unknown, [17933]}
[28789]  {:unknown, [17931]}
[28790]  {:unknown, [17998]}
[28791]  {:unknown, [17946]}
[28792]  {:unknown, [17926]}
[28793]  {:unknown, [17931]}
[28794]  {:unknown, [17998]}
[28795]  {:unknown, [17922]}
[28796]  {:unknown, [17935]}
[28797]  {:unknown, [17949]}
[28798]  {:unknown, [17946]}
[28799]  {:unknown, [17998]}
[28800]  {:unknown, [17933]}
[28801]  {:unknown, [17921]}
[28802]  {:unknown, [17927]}
[28803]  {:unknown, [17920]}
[28804]  {:unknown, [17986]}
[28805]  {:unknown, [17998]}
[28806]  {:unknown, [17943]}
[28807]  {:unknown, [17921]}
[28808]  {:unknown, [17947]}
[28809]  {:unknown, [17998]}
[28810]  {:unknown, [17926]}
[28811]  {:unknown, [17931]}
[28812]  {:unknown, [17935]}
[28813]  {:unknown, [17948]}
[28814]  {:unknown, [17998]}
[28815]  {:unknown, [17935]}
[28816]  {:unknown, [17998]}
[28817]  {:unknown, [17933]}
[28818]  {:unknown, [17922]}
[28819]  {:unknown, [17927]}
[28820]  {:unknown, [17933]}
[28821]  {:unknown, [17925]}
[28822]  {:unknown, [17998]}
[28823]  {:unknown, [17928]}
[28824]  {:unknown, [17948]}
[28825]  {:unknown, [17921]}
[28826]  {:unknown, [17923]}
[28827]  {:unknown, [17998]}
[28828]  {:unknown, [17946]}
[28829]  {:unknown, [17926]}
[28830]  {:unknown, [17931]}
[28831]  {:unknown, [17998]}
[28832]  {:unknown, [17920]}
[28833]  {:unknown, [17921]}
[28834]  {:unknown, [17948]}
[28835]  {:unknown, [17946]}
[28836]  {:unknown, [17926]}
[28837]  {:unknown, [17998]}
[28838]  {:unknown, [17930]}
[28839]  {:unknown, [17921]}
[28840]  {:unknown, [17921]}
[28841]  {:unknown, [17948]}
[28842]  {:unknown, [17984]}
[28843]  {:unknown, [18020]}
[28844]  {:unknown, [169]}				# "Start of encrypted teleporter message"
[28845]  {:unknown, [14243]}
[28846]  {:unknown, [14274]}
[28847]  {:unknown, [14225]}
[28848]  {:unknown, [14230]}
[28849]  {:unknown, [14224]}
[28850]  {:unknown, [14211]}
[28851]  {:unknown, [14220]}
[28852]  {:unknown, [14213]}
[28853]  {:unknown, [14215]}
[28854]  {:unknown, [14286]}
[28855]  {:unknown, [14274]}
[28856]  {:unknown, [14215]}
[28857]  {:unknown, [14222]}
[28858]  {:unknown, [14215]}
[28859]  {:unknown, [14209]}
[28860]  {:unknown, [14230]}
[28861]  {:unknown, [14224]}
[28862]  {:unknown, [14221]}
[28863]  {:unknown, [14220]}
[28864]  {:unknown, [14219]}
[28865]  {:unknown, [14209]}
[28866]  {:unknown, [14274]}
[28867]  {:unknown, [14228]}
[28868]  {:unknown, [14221]}
[28869]  {:unknown, [14219]}
[28870]  {:unknown, [14209]}
[28871]  {:unknown, [14215]}
[28872]  {:unknown, [14274]}
[28873]  {:unknown, [14219]}
[28874]  {:unknown, [14225]}
[28875]  {:unknown, [14274]}
[28876]  {:unknown, [14226]}
[28877]  {:unknown, [14224]}
[28878]  {:unknown, [14221]}
[28879]  {:unknown, [14216]}
[28880]  {:unknown, [14215]}
[28881]  {:unknown, [14209]}
[28882]  {:unknown, [14230]}
[28883]  {:unknown, [14215]}
[28884]  {:unknown, [14214]}
[28885]  {:unknown, [14274]}
[28886]  {:unknown, [14219]}
[28887]  {:unknown, [14220]}
[28888]  {:unknown, [14230]}
[28889]  {:unknown, [14221]}
[28890]  {:unknown, [14274]}
[28891]  {:unknown, [14235]}
[28892]  {:unknown, [14221]}
[28893]  {:unknown, [14231]}
[28894]  {:unknown, [14224]}
[28895]  {:unknown, [14274]}
[28896]  {:unknown, [14223]}
[28897]  {:unknown, [14219]}
[28898]  {:unknown, [14220]}
[28899]  {:unknown, [14214]}
[28900]  {:unknown, [14296]}
[28901]  {:unknown, [14312]}
[28902]  {:unknown, [14312]}
[28903]  {:unknown, [14274]}
[28904]  {:unknown, [14274]}
[28905]  {:unknown, [14272]}
[28906]  {:unknown, [14263]}
[28907]  {:unknown, [14220]}
[28908]  {:unknown, [14231]}
[28909]  {:unknown, [14225]}
[28910]  {:unknown, [14231]}
[28911]  {:unknown, [14211]}
[28912]  {:unknown, [14222]}
[28913]  {:unknown, [14274]}
[28914]  {:unknown, [14225]}
[28915]  {:unknown, [14215]}
[28916]  {:unknown, [14230]}
[28917]  {:unknown, [14230]}
[28918]  {:unknown, [14219]}
[28919]  {:unknown, [14220]}
[28920]  {:unknown, [14213]}
[28921]  {:unknown, [14274]}
[28922]  {:unknown, [14214]}
[28923]  {:unknown, [14215]}
[28924]  {:unknown, [14230]}
[28925]  {:unknown, [14215]}
[28926]  {:unknown, [14209]}
[28927]  {:unknown, [14230]}
[28928]  {:unknown, [14215]}
[28929]  {:unknown, [14214]}
[28930]  {:unknown, [14275]}
[28931]  {:unknown, [14274]}
[28932]  {:unknown, [14274]}
[28933]  {:unknown, [14257]}
[28934]  {:unknown, [14230]}
[28935]  {:unknown, [14211]}
[28936]  {:unknown, [14224]}
[28937]  {:unknown, [14230]}
[28938]  {:unknown, [14219]}
[28939]  {:unknown, [14220]}
[28940]  {:unknown, [14213]}
[28941]  {:unknown, [14274]}
[28942]  {:unknown, [14209]}
[28943]  {:unknown, [14221]}
[28944]  {:unknown, [14220]}
[28945]  {:unknown, [14212]}
[28946]  {:unknown, [14219]}
[28947]  {:unknown, [14224]}
[28948]  {:unknown, [14223]}
[28949]  {:unknown, [14211]}
[28950]  {:unknown, [14230]}
[28951]  {:unknown, [14219]}
[28952]  {:unknown, [14221]}
[28953]  {:unknown, [14220]}
[28954]  {:unknown, [14274]}
[28955]  {:unknown, [14226]}
[28956]  {:unknown, [14224]}
[28957]  {:unknown, [14221]}
[28958]  {:unknown, [14209]}
[28959]  {:unknown, [14215]}
[28960]  {:unknown, [14225]}
[28961]  {:unknown, [14225]}
[28962]  {:unknown, [14275]}
[28963]  {:unknown, [14274]}
[28964]  {:unknown, [14274]}
[28965]  {:unknown, [14247]}
[28966]  {:unknown, [14225]}
[28967]  {:unknown, [14230]}
[28968]  {:unknown, [14219]}
[28969]  {:unknown, [14223]}
[28970]  {:unknown, [14211]}
[28971]  {:unknown, [14230]}
[28972]  {:unknown, [14215]}
[28973]  {:unknown, [14214]}
[28974]  {:unknown, [14274]}
[28975]  {:unknown, [14230]}
[28976]  {:unknown, [14219]}
[28977]  {:unknown, [14223]}
[28978]  {:unknown, [14215]}
[28979]  {:unknown, [14274]}
[28980]  {:unknown, [14230]}
[28981]  {:unknown, [14221]}
[28982]  {:unknown, [14274]}
[28983]  {:unknown, [14209]}
[28984]  {:unknown, [14221]}
[28985]  {:unknown, [14223]}
[28986]  {:unknown, [14226]}
[28987]  {:unknown, [14222]}
[28988]  {:unknown, [14215]}
[28989]  {:unknown, [14230]}
[28990]  {:unknown, [14219]}
[28991]  {:unknown, [14221]}
[28992]  {:unknown, [14220]}
[28993]  {:unknown, [14296]}
[28994]  {:unknown, [14274]}
[28995]  {:unknown, [14291]}
[28996]  {:unknown, [14274]}
[28997]  {:unknown, [14208]}
[28998]  {:unknown, [14219]}
[28999]  {:unknown, [14222]}
[29000]  {:unknown, [14222]}
[29001]  {:unknown, [14219]}
[29002]  {:unknown, [14221]}
[29003]  {:unknown, [14220]}
[29004]  {:unknown, [14274]}
[29005]  {:unknown, [14235]}
[29006]  {:unknown, [14215]}
[29007]  {:unknown, [14211]}
[29008]  {:unknown, [14224]}
[29009]  {:unknown, [14225]}
[29010]  {:unknown, [14284]}
[29011]  {:unknown, [14272]}
[29012]  {:unknown, [14312]}
[29013]  {:unknown, [14312]}
[29014]  {:unknown, [226]}
[29015]  {:unknown, [9642]}
[29016]  {:unknown, [9628]}
[29017]  {:unknown, [9606]}
[29018]  {:unknown, [9683]}
[29019]  {:unknown, [9604]}
[29020]  {:unknown, [9618]}
[29021]  {:unknown, [9624]}
[29022]  {:unknown, [9622]}
[29023]  {:unknown, [9683]}
[29024]  {:unknown, [9606]}
[29025]  {:unknown, [9603]}
[29026]  {:unknown, [9683]}
[29027]  {:unknown, [9628]}
[29028]  {:unknown, [9629]}
[29029]  {:unknown, [9683]}
[29030]  {:unknown, [9618]}
[29031]  {:unknown, [9683]}
[29032]  {:unknown, [9600]}
[29033]  {:unknown, [9618]}
[29034]  {:unknown, [9629]}
[29035]  {:unknown, [9623]}
[29036]  {:unknown, [9610]}
[29037]  {:unknown, [9683]}
[29038]  {:unknown, [9617]}
[29039]  {:unknown, [9622]}
[29040]  {:unknown, [9618]}
[29041]  {:unknown, [9616]}
[29042]  {:unknown, [9627]}
[29043]  {:unknown, [9683]}
[29044]  {:unknown, [9604]}
[29045]  {:unknown, [9626]}
[29046]  {:unknown, [9607]}
[29047]  {:unknown, [9627]}
[29048]  {:unknown, [9683]}
[29049]  {:unknown, [9618]}
[29050]  {:unknown, [9683]}
[29051]  {:unknown, [9600]}
[29052]  {:unknown, [9631]}
[29053]  {:unknown, [9626]}
[29054]  {:unknown, [9620]}
[29055]  {:unknown, [9627]}
[29056]  {:unknown, [9607]}
[29057]  {:unknown, [9683]}
[29058]  {:unknown, [9627]}
[29059]  {:unknown, [9622]}
[29060]  {:unknown, [9618]}
[29061]  {:unknown, [9623]}
[29062]  {:unknown, [9618]}
[29063]  {:unknown, [9616]}
[29064]  {:unknown, [9627]}
[29065]  {:unknown, [9622]}
[29066]  {:unknown, [9693]}
[29067]  {:unknown, [9683]}
[29068]  {:unknown, [9683]}
[29069]  {:unknown, [9639]}
[29070]  {:unknown, [9627]}
[29071]  {:unknown, [9622]}
[29072]  {:unknown, [9683]}
[29073]  {:unknown, [9631]}
[29074]  {:unknown, [9618]}
[29075]  {:unknown, [9600]}
[29076]  {:unknown, [9607]}
[29077]  {:unknown, [9683]}
[29078]  {:unknown, [9607]}
[29079]  {:unknown, [9627]}
[29080]  {:unknown, [9626]}
[29081]  {:unknown, [9629]}
[29082]  {:unknown, [9620]}
[29083]  {:unknown, [9683]}
[29084]  {:unknown, [9610]}
[29085]  {:unknown, [9628]}
[29086]  {:unknown, [9606]}
[29087]  {:unknown, [9683]}
[29088]  {:unknown, [9601]}
[29089]  {:unknown, [9622]}
[29090]  {:unknown, [9630]}
[29091]  {:unknown, [9622]}
[29092]  {:unknown, [9630]}
[29093]  {:unknown, [9617]}
[29094]  {:unknown, [9622]}
[29095]  {:unknown, [9601]}
[29096]  {:unknown, [9683]}
[29097]  {:unknown, [9626]}
[29098]  {:unknown, [9600]}
[29099]  {:unknown, [9683]}
[29100]  {:unknown, [9618]}
[29101]  {:unknown, [9616]}
[29102]  {:unknown, [9607]}
[29103]  {:unknown, [9626]}
[29104]  {:unknown, [9605]}
[29105]  {:unknown, [9618]}
[29106]  {:unknown, [9607]}
[29107]  {:unknown, [9626]}
[29108]  {:unknown, [9629]}
[29109]  {:unknown, [9620]}
[29110]  {:unknown, [9683]}
[29111]  {:unknown, [9607]}
[29112]  {:unknown, [9627]}
[29113]  {:unknown, [9618]}
[29114]  {:unknown, [9607]}
[29115]  {:unknown, [9683]}
[29116]  {:unknown, [9607]}
[29117]  {:unknown, [9622]}
[29118]  {:unknown, [9631]}
[29119]  {:unknown, [9622]}
[29120]  {:unknown, [9603]}
[29121]  {:unknown, [9628]}
[29122]  {:unknown, [9601]}
[29123]  {:unknown, [9607]}
[29124]  {:unknown, [9622]}
[29125]  {:unknown, [9601]}
[29126]  {:unknown, [9693]}
[29127]  {:unknown, [9693]}
[29128]  {:unknown, [9693]}
[29129]  {:unknown, [9683]}
[29130]  {:unknown, [9617]}
[29131]  {:unknown, [9606]}
[29132]  {:unknown, [9607]}
[29133]  {:unknown, [9683]}
[29134]  {:unknown, [9629]}
[29135]  {:unknown, [9628]}
[29136]  {:unknown, [9604]}
[29137]  {:unknown, [9683]}
[29138]  {:unknown, [9610]}
[29139]  {:unknown, [9628]}
[29140]  {:unknown, [9606]}
[29141]  {:unknown, [9683]}
[29142]  {:unknown, [9616]}
[29143]  {:unknown, [9618]}
[29144]  {:unknown, [9629]}
[29145]  {:unknown, [9684]}
[29146]  {:unknown, [9607]}
[29147]  {:unknown, [9683]}
[29148]  {:unknown, [9621]}
[29149]  {:unknown, [9626]}
[29150]  {:unknown, [9629]}
[29151]  {:unknown, [9623]}
[29152]  {:unknown, [9683]}
[29153]  {:unknown, [9626]}
[29154]  {:unknown, [9607]}
[29155]  {:unknown, [9683]}
[29156]  {:unknown, [9618]}
[29157]  {:unknown, [9629]}
[29158]  {:unknown, [9610]}
[29159]  {:unknown, [9604]}
[29160]  {:unknown, [9627]}
[29161]  {:unknown, [9622]}
[29162]  {:unknown, [9601]}
[29163]  {:unknown, [9622]}
[29164]  {:unknown, [9683]}
[29165]  {:unknown, [9626]}
[29166]  {:unknown, [9629]}
[29167]  {:unknown, [9683]}
[29168]  {:unknown, [9610]}
[29169]  {:unknown, [9628]}
[29170]  {:unknown, [9606]}
[29171]  {:unknown, [9601]}
[29172]  {:unknown, [9683]}
[29173]  {:unknown, [9603]}
[29174]  {:unknown, [9618]}
[29175]  {:unknown, [9616]}
[29176]  {:unknown, [9624]}
[29177]  {:unknown, [9693]}
[29178]  {:unknown, [9683]}
[29179]  {:unknown, [9683]}
[29180]  {:unknown, [9632]}
[29181]  {:unknown, [9628]}
[29182]  {:unknown, [9630]}
[29183]  {:unknown, [9622]}
[29184]  {:unknown, [9628]}
[29185]  {:unknown, [9629]}
[29186]  {:unknown, [9622]}
[29187]  {:unknown, [9683]}
[29188]  {:unknown, [9600]}
[29189]  {:unknown, [9622]}
[29190]  {:unknown, [9622]}
[29191]  {:unknown, [9630]}
[29192]  {:unknown, [9600]}
[29193]  {:unknown, [9683]}
[29194]  {:unknown, [9607]}
[29195]  {:unknown, [9628]}
[29196]  {:unknown, [9683]}
[29197]  {:unknown, [9627]}
[29198]  {:unknown, [9618]}
[29199]  {:unknown, [9605]}
[29200]  {:unknown, [9622]}
[29201]  {:unknown, [9683]}
[29202]  {:unknown, [9623]}
[29203]  {:unknown, [9601]}
[29204]  {:unknown, [9618]}
[29205]  {:unknown, [9604]}
[29206]  {:unknown, [9629]}
[29207]  {:unknown, [9683]}
[29208]  {:unknown, [9618]}
[29209]  {:unknown, [9683]}
[29210]  {:unknown, [9630]}
[29211]  {:unknown, [9622]}
[29212]  {:unknown, [9600]}
[29213]  {:unknown, [9600]}
[29214]  {:unknown, [9618]}
[29215]  {:unknown, [9620]}
[29216]  {:unknown, [9622]}
[29217]  {:unknown, [9683]}
[29218]  {:unknown, [9626]}
[29219]  {:unknown, [9629]}
[29220]  {:unknown, [9683]}
[29221]  {:unknown, [9607]}
[29222]  {:unknown, [9627]}
[29223]  {:unknown, [9622]}
[29224]  {:unknown, [9683]}
[29225]  {:unknown, [9600]}
[29226]  {:unknown, [9618]}
[29227]  {:unknown, [9629]}
[29228]  {:unknown, [9623]}
[29229]  {:unknown, [9683]}
[29230]  {:unknown, [9627]}
[29231]  {:unknown, [9622]}
[29232]  {:unknown, [9601]}
[29233]  {:unknown, [9622]}
[29234]  {:unknown, [9673]}
[29235]  {:unknown, [9721]}
[29236]  {:unknown, [9721]}
[29237]  {:unknown, [9683]}
[29238]  {:unknown, [9683]}
[29239]  {:unknown, [9683]}
[29240]  {:unknown, [9683]}
[29241]  {:pop, {:value, 10492}}
[29243]  {:unknown, [14825]}
[29244]  {:unknown, [19276]}
[29245]  {:unknown, [154]}
[29246]  {:unknown, [27235]}
[29247]  {:unknown, [27235]}
[29248]  {:unknown, [27168]}
[29249]  {:unknown, [27165]}
[29250]  {:unknown, [27209]}
[29251]  {:unknown, [27147]}
[29252]  {:unknown, [27148]}
[29253]  {:unknown, [27150]}
[29254]  {:unknown, [27136]}
[29255]  {:unknown, [27143]}
[29256]  {:unknown, [27162]}
[29257]  {:unknown, [27209]}
[29258]  {:unknown, [27165]}
[29259]  {:unknown, [27142]}
[29260]  {:unknown, [27209]}
[29261]  {:unknown, [27163]}
[29262]  {:unknown, [27144]}
[29263]  {:unknown, [27136]}
[29264]  {:unknown, [27143]}
[29265]  {:unknown, [27207]}
[29266]  {:unknown, [27209]}
[29267]  {:unknown, [27209]}
[29268]  {:unknown, [27197]}
[29269]  {:unknown, [27137]}
[29270]  {:unknown, [27148]}
[29271]  {:unknown, [27209]}
[29272]  {:unknown, [27140]}
[29273]  {:unknown, [27148]}
[29274]  {:unknown, [27162]}
[29275]  {:unknown, [27162]}
[29276]  {:unknown, [27144]}
[29277]  {:unknown, [27150]}
[29278]  {:unknown, [27148]}
[29279]  {:unknown, [27209]}
[29280]  {:unknown, [27166]}
[29281]  {:unknown, [27144]}
[29282]  {:unknown, [27162]}
[29283]  {:unknown, [27137]}
[29284]  {:unknown, [27148]}
[29285]  {:unknown, [27162]}
[29286]  {:unknown, [27209]}
[29287]  {:unknown, [27144]}
[29288]  {:unknown, [27166]}
[29289]  {:unknown, [27144]}
[29290]  {:unknown, [27152]}
[29291]  {:unknown, [27207]}
[29292]  {:unknown, [27209]}
[29293]  {:unknown, [27209]}
[29294]  {:unknown, [27184]}
[29295]  {:unknown, [27142]}
[29296]  {:unknown, [27164]}
[29297]  {:unknown, [27209]}
[29298]  {:unknown, [27165]}
[29299]  {:unknown, [27144]}
[29300]  {:unknown, [27138]}
[29301]  {:unknown, [27148]}
[29302]  {:unknown, [27209]}
[29303]  {:unknown, [27144]}
[29304]  {:unknown, [27209]}
[29305]  {:unknown, [27149]}
[29306]  {:unknown, [27148]}
[29307]  {:unknown, [27148]}
[29308]  {:unknown, [27161]}
[29309]  {:unknown, [27209]}
[29310]  {:unknown, [27147]}
[29311]  {:unknown, [27163]}
[29312]  {:unknown, [27148]}
[29313]  {:unknown, [27144]}
[29314]  {:unknown, [27165]}
[29315]  {:unknown, [27137]}
[29316]  {:unknown, [27209]}
[29317]  {:unknown, [27144]}
[29318]  {:unknown, [27143]}
[29319]  {:unknown, [27149]}
[29320]  {:unknown, [27209]}
[29321]  {:unknown, [27151]}
[29322]  {:unknown, [27148]}
[29323]  {:unknown, [27148]}
[29324]  {:unknown, [27141]}
[29325]  {:unknown, [27209]}
[29326]  {:unknown, [27151]}
[29327]  {:unknown, [27136]}
[29328]  {:unknown, [27163]}
[29329]  {:unknown, [27140]}
[29330]  {:unknown, [27141]}
[29331]  {:unknown, [27152]}
[29332]  {:unknown, [27209]}
[29333]  {:unknown, [27150]}
[29334]  {:unknown, [27163]}
[29335]  {:unknown, [27142]}
[29336]  {:unknown, [27164]}
[29337]  {:unknown, [27143]}
[29338]  {:unknown, [27149]}
[29339]  {:unknown, [27148]}
[29340]  {:unknown, [27149]}
[29341]  {:unknown, [27209]}
[29342]  {:unknown, [27136]}
[29343]  {:unknown, [27143]}
[29344]  {:unknown, [27209]}
[29345]  {:unknown, [27163]}
[29346]  {:unknown, [27148]}
[29347]  {:unknown, [27144]}
[29348]  {:unknown, [27141]}
[29349]  {:unknown, [27136]}
[29350]  {:unknown, [27165]}
[29351]  {:unknown, [27152]}
[29352]  {:unknown, [27209]}
[29353]  {:unknown, [27144]}
[29354]  {:unknown, [27162]}
[29355]  {:unknown, [27209]}
[29356]  {:unknown, [27165]}
[29357]  {:unknown, [27137]}
[29358]  {:unknown, [27148]}
[29359]  {:unknown, [27209]}
[29360]  {:unknown, [27148]}
[29361]  {:unknown, [27151]}
[29362]  {:unknown, [27151]}
[29363]  {:unknown, [27148]}
[29364]  {:unknown, [27146]}
[29365]  {:unknown, [27165]}
[29366]  {:unknown, [27162]}
[29367]  {:unknown, [27209]}
[29368]  {:unknown, [27142]}
[29369]  {:unknown, [27151]}
[29370]  {:unknown, [27209]}
[29371]  {:unknown, [27165]}
[29372]  {:unknown, [27137]}
[29373]  {:unknown, [27148]}
[29374]  {:unknown, [27209]}
[29375]  {:unknown, [27165]}
[29376]  {:unknown, [27148]}
[29377]  {:unknown, [27141]}
[29378]  {:unknown, [27148]}
[29379]  {:unknown, [27161]}
[29380]  {:unknown, [27142]}
[29381]  {:unknown, [27163]}
[29382]  {:unknown, [27165]}
[29383]  {:unknown, [27144]}
[29384]  {:unknown, [27165]}
[29385]  {:unknown, [27136]}
[29386]  {:unknown, [27142]}
[29387]  {:unknown, [27143]}
[29388]  {:unknown, [27209]}
[29389]  {:unknown, [27166]}
[29390]  {:unknown, [27148]}
[29391]  {:unknown, [27144]}
[29392]  {:unknown, [27163]}
[29393]  {:unknown, [27209]}
[29394]  {:unknown, [27142]}
[29395]  {:unknown, [27151]}
[29396]  {:unknown, [27151]}
[29397]  {:unknown, [27207]}
[29398]  {:unknown, [27235]}
[29399]  {:unknown, [27235]}
[29400]  {:unknown, [144]}
[29401]  {:unknown, [28971]}
[29402]  {:unknown, [29002]}
[29403]  {:unknown, [28953]}
[29404]  {:unknown, [28958]}
[29405]  {:unknown, [28952]}
[29406]  {:unknown, [28939]}
[29407]  {:unknown, [28932]}
[29408]  {:unknown, [28941]}
[29409]  {:unknown, [28943]}
[29410]  {:unknown, [28998]}
[29411]  {:unknown, [29002]}
[29412]  {:unknown, [28943]}
[29413]  {:unknown, [28934]}
[29414]  {:unknown, [28943]}
[29415]  {:unknown, [28937]}
[29416]  {:unknown, [28958]}
[29417]  {:unknown, [28952]}
[29418]  {:unknown, [28933]}
[29419]  {:unknown, [28932]}
[29420]  {:unknown, [28931]}
[29421]  {:unknown, [28937]}
[29422]  {:unknown, [29002]}
[29423]  {:unknown, [28956]}
[29424]  {:unknown, [28933]}
[29425]  {:unknown, [28931]}
[29426]  {:unknown, [28937]}
[29427]  {:unknown, [28943]}
[29428]  {:unknown, [29002]}
[29429]  {:unknown, [28931]}
[29430]  {:unknown, [28953]}
[29431]  {:unknown, [29002]}
[29432]  {:unknown, [28954]}
[29433]  {:unknown, [28952]}
[29434]  {:unknown, [28933]}
[29435]  {:unknown, [28928]}
[29436]  {:unknown, [28943]}
[29437]  {:unknown, [28937]}
[29438]  {:unknown, [28958]}
[29439]  {:unknown, [28943]}
[29440]  {:unknown, [28942]}
[29441]  {:unknown, [29002]}
[29442]  {:unknown, [28931]}
[29443]  {:unknown, [28932]}
[29444]  {:unknown, [28958]}
[29445]  {:unknown, [28933]}
[29446]  {:unknown, [29002]}
[29447]  {:unknown, [28947]}
[29448]  {:unknown, [28933]}
[29449]  {:unknown, [28959]}
[29450]  {:unknown, [28952]}
[29451]  {:unknown, [29002]}
[29452]  {:unknown, [28935]}
[29453]  {:unknown, [28931]}
[29454]  {:unknown, [28932]}
[29455]  {:unknown, [28942]}
[29456]  {:unknown, [29008]}
[29457]  {:unknown, [29024]}
[29458]  {:unknown, [29024]}
[29459]  {:unknown, [29002]}
[29460]  {:unknown, [29002]}
[29461]  {:unknown, [29000]}
[29462]  {:unknown, [28967]}
[29463]  {:unknown, [28931]}
[29464]  {:unknown, [28953]}
[29465]  {:unknown, [28937]}
[29466]  {:unknown, [28939]}
[29467]  {:unknown, [28934]}
[29468]  {:unknown, [28931]}
[29469]  {:unknown, [28936]}
[29470]  {:unknown, [28952]}
[29471]  {:unknown, [28939]}
[29472]  {:unknown, [28958]}
[29473]  {:unknown, [28931]}
[29474]  {:unknown, [28933]}
[29475]  {:unknown, [28932]}
[29476]  {:unknown, [29002]}
[29477]  {:unknown, [28942]}
[29478]  {:unknown, [28943]}
[29479]  {:unknown, [28958]}
[29480]  {:unknown, [28943]}
[29481]  {:unknown, [28937]}
[29482]  {:unknown, [28958]}
[29483]  {:unknown, [28943]}
[29484]  {:unknown, [28942]}
[29485]  {:unknown, [29003]}
[29486]  {:unknown, [29002]}
[29487]  {:unknown, [29002]}
[29488]  {:unknown, [28971]}
[29489]  {:unknown, [28936]}
[29490]  {:unknown, [28933]}
[29491]  {:unknown, [28952]}
[29492]  {:unknown, [28958]}
[29493]  {:unknown, [28931]}
[29494]  {:unknown, [28932]}
[29495]  {:unknown, [28941]}
[29496]  {:unknown, [29002]}
[29497]  {:unknown, [28958]}
[29498]  {:unknown, [28943]}
[29499]  {:unknown, [28934]}
[29500]  {:unknown, [28943]}
[29501]  {:unknown, [28954]}
[29502]  {:unknown, [28933]}
[29503]  {:unknown, [28952]}
[29504]  {:unknown, [28958]}
[29505]  {:unknown, [28939]}
[29506]  {:unknown, [28958]}
[29507]  {:unknown, [28931]}
[29508]  {:unknown, [28933]}
[29509]  {:unknown, [28932]}
[29510]  {:unknown, [29003]}
[29511]  {:unknown, [29000]}
[29512]  {:unknown, [29024]}
[29513]  {:unknown, [29024]}
[29514]  {:unknown, [28964]}
[29515]  {:unknown, [28933]}
[29516]  {:unknown, [28958]}
[29517]  {:unknown, [28930]}
[29518]  {:unknown, [28931]}
[29519]  {:unknown, [28932]}
[29520]  {:unknown, [28941]}
[29521]  {:unknown, [29002]}
[29522]  {:unknown, [28943]}
[29523]  {:unknown, [28934]}
[29524]  {:unknown, [28953]}
[29525]  {:unknown, [28943]}
[29526]  {:unknown, [29002]}
[29527]  {:unknown, [28953]}
[29528]  {:unknown, [28943]}
[29529]  {:unknown, [28943]}
[29530]  {:unknown, [28935]}
[29531]  {:unknown, [28953]}
[29532]  {:unknown, [29002]}
[29533]  {:unknown, [28958]}
[29534]  {:unknown, [28933]}
[29535]  {:unknown, [29002]}
[29536]  {:unknown, [28930]}
[29537]  {:unknown, [28939]}
[29538]  {:unknown, [28954]}
[29539]  {:unknown, [28954]}
[29540]  {:unknown, [28943]}
[29541]  {:unknown, [28932]}
[29542]  {:unknown, [28996]}
[29543]  {:unknown, [29024]}
[29544]  {:unknown, [29024]}
[29545]  {:unknown, 'u'}
[29546]  {:unknown, [26683]}
[29547]  {:unknown, [26637]}
[29548]  {:unknown, [26647]}
[29549]  {:unknown, [26690]}
[29550]  {:unknown, [26627]}
[29551]  {:unknown, [26625]}
[29552]  {:unknown, [26646]}
[29553]  {:unknown, [26635]}
[29554]  {:unknown, [26644]}
[29555]  {:unknown, [26627]}
[29556]  {:unknown, [26646]}
[29557]  {:unknown, [26631]}
[29558]  {:unknown, [26690]}
[29559]  {:unknown, [26646]}
[29560]  {:unknown, [26634]}
[29561]  {:unknown, [26631]}
[29562]  {:unknown, [26690]}
[29563]  {:unknown, [26646]}
[29564]  {:unknown, [26631]}
[29565]  {:unknown, [26638]}
[29566]  {:unknown, [26631]}
[29567]  {:unknown, [26642]}
[29568]  {:unknown, [26637]}
[29569]  {:unknown, [26640]}
[29570]  {:unknown, [26646]}
[29571]  {:unknown, [26631]}
[29572]  {:unknown, [26640]}
[29573]  {:unknown, [26691]}
[29574]  {:unknown, [26690]}
[29575]  {:unknown, [26690]}
[29576]  {:unknown, [26659]}
[29577]  {:unknown, [26641]}
[29578]  {:unknown, [26690]}
[29579]  {:unknown, [26651]}
[29580]  {:unknown, [26637]}
[29581]  {:unknown, [26647]}
[29582]  {:unknown, [26690]}
[29583]  {:unknown, [26641]}
[29584]  {:unknown, [26642]}
[29585]  {:unknown, [26635]}
[29586]  {:unknown, [26640]}
[29587]  {:unknown, [26627]}
[29588]  {:unknown, [26638]}
[29589]  {:unknown, [26690]}
[29590]  {:unknown, [26646]}
[29591]  {:unknown, [26634]}
[29592]  {:unknown, [26640]}
[29593]  {:unknown, [26637]}
[29594]  {:unknown, [26647]}
[29595]  {:unknown, [26629]}
[29596]  {:unknown, [26634]}
[29597]  {:unknown, [26690]}
[29598]  {:unknown, [26646]}
[29599]  {:unknown, [26635]}
[29600]  {:unknown, [26639]}
[29601]  {:unknown, [26631]}
[29602]  {:unknown, [26690]}
[29603]  {:unknown, [26627]}
[29604]  {:unknown, [26636]}
[29605]  {:unknown, [26630]}
[29606]  {:unknown, [26690]}
[29607]  {:unknown, [26641]}
[29608]  {:unknown, [26642]}
[29609]  {:unknown, [26627]}
[29610]  {:unknown, [26625]}
[29611]  {:unknown, [26631]}
[29612]  {:unknown, [26702]}
[29613]  {:unknown, [26690]}
[29614]  {:unknown, [26651]}
[29615]  {:unknown, [26637]}
[29616]  {:unknown, [26647]}
[29617]  {:unknown, [26690]}
[29618]  {:unknown, [26646]}
[29619]  {:unknown, [26634]}
[29620]  {:unknown, [26635]}
[29621]  {:unknown, [26636]}
[29622]  {:unknown, [26633]}
[29623]  {:unknown, [26690]}
[29624]  {:unknown, [26651]}
[29625]  {:unknown, [26637]}
[29626]  {:unknown, [26647]}
[29627]  {:unknown, [26690]}
[29628]  {:unknown, [26641]}
[29629]  {:unknown, [26631]}
[29630]  {:unknown, [26631]}
[29631]  {:unknown, [26690]}
[29632]  {:unknown, [26627]}
[29633]  {:unknown, [26690]}
[29634]  {:unknown, [26642]}
[29635]  {:unknown, [26627]}
[29636]  {:unknown, [26646]}
[29637]  {:unknown, [26646]}
[29638]  {:unknown, [26631]}
[29639]  {:unknown, [26640]}
[29640]  {:unknown, [26636]}
[29641]  {:unknown, [26690]}
[29642]  {:unknown, [26635]}
[29643]  {:unknown, [26636]}
[29644]  {:unknown, [26690]}
[29645]  {:unknown, [26646]}
[29646]  {:unknown, [26634]}
[29647]  {:unknown, [26631]}
[29648]  {:unknown, [26690]}
[29649]  {:unknown, [26641]}
[29650]  {:unknown, [26646]}
[29651]  {:unknown, [26627]}
[29652]  {:unknown, [26640]}
[29653]  {:unknown, [26641]}
[29654]  {:unknown, [26700]}
[29655]  {:unknown, [26700]}
[29656]  {:unknown, [26700]}
[29657]  {:unknown, [26728]}
[29658]  {:unknown, [26728]}
[29659]  {:unknown, [26690]}
[29660]  {:unknown, [26690]}
[29661]  {:unknown, [26690]}
[29662]  {:unknown, [26690]}
[29663]  {:pop, {:value, 12686}}
[29665]  {:unknown, [30145]}
[29666]  {:unknown, [17093]}
[29667]  {:unknown, 'Y'}
[29668]  {:unknown, [27877]}
[29669]  {:unknown, [27877]}
[29670]  {:unknown, [27822]}
[29671]  {:unknown, [27785]}
[29672]  {:unknown, [27803]}
[29673]  {:unknown, [27786]}
[29674]  {:unknown, [27805]}
[29675]  {:unknown, [27855]}
[29676]  {:unknown, [27790]}
[29677]  {:unknown, [27855]}
[29678]  {:unknown, [27785]}
[29679]  {:unknown, [27786]}
[29680]  {:unknown, [27800]}
[29681]  {:unknown, [27855]}
[29682]  {:unknown, [27778]}
[29683]  {:unknown, [27776]}
[29684]  {:unknown, [27778]}
[29685]  {:unknown, [27786]}
[29686]  {:unknown, [27777]}
[29687]  {:unknown, [27803]}
[29688]  {:unknown, [27804]}
[29689]  {:unknown, [27843]}
[29690]  {:unknown, [27855]}
[29691]  {:unknown, [27798]}
[29692]  {:unknown, [27776]}
[29693]  {:unknown, [27802]}
[29694]  {:unknown, [27855]}
[29695]  {:unknown, [27785]}
[29696]  {:unknown, [27782]}
[29697]  {:unknown, [27777]}
[29698]  {:unknown, [27787]}
[29699]  {:unknown, [27855]}
[29700]  {:unknown, [27798]}
[29701]  {:unknown, [27776]}
[29702]  {:unknown, [27802]}
[29703]  {:unknown, [27805]}
[29704]  {:unknown, [27804]}
[29705]  {:unknown, [27786]}
[29706]  {:unknown, [27779]}
[29707]  {:unknown, [27785]}
[29708]  {:unknown, [27855]}
[29709]  {:unknown, [27789]}
[29710]  {:unknown, [27790]}
[29711]  {:unknown, [27788]}
[29712]  {:unknown, [27780]}
[29713]  {:unknown, [27855]}
[29714]  {:unknown, [27776]}
[29715]  {:unknown, [27777]}
[29716]  {:unknown, [27855]}
[29717]  {:unknown, [27804]}
[29718]  {:unknown, [27776]}
[29719]  {:unknown, [27779]}
[29720]  {:unknown, [27782]}
[29721]  {:unknown, [27787]}
[29722]  {:unknown, [27855]}
[29723]  {:unknown, [27784]}
[29724]  {:unknown, [27805]}
[29725]  {:unknown, [27776]}
[29726]  {:unknown, [27802]}
[29727]  {:unknown, [27777]}
[29728]  {:unknown, [27787]}
[29729]  {:unknown, [27855]}
[29730]  {:unknown, [27790]}
[29731]  {:unknown, [27777]}
[29732]  {:unknown, [27787]}
[29733]  {:unknown, [27855]}
[29734]  {:unknown, [27790]}
[29735]  {:unknown, [27855]}
[29736]  {:unknown, [27779]}
[29737]  {:unknown, [27782]}
[29738]  {:unknown, [27803]}
[29739]  {:unknown, [27803]}
[29740]  {:unknown, [27779]}
[29741]  {:unknown, [27786]}
[29742]  {:unknown, [27855]}
[29743]  {:unknown, [27787]}
[29744]  {:unknown, [27782]}
[29745]  {:unknown, [27804]}
[29746]  {:unknown, [27776]}
[29747]  {:unknown, [27805]}
[29748]  {:unknown, [27782]}
[29749]  {:unknown, [27786]}
[29750]  {:unknown, [27777]}
[29751]  {:unknown, [27803]}
[29752]  {:unknown, [27786]}
[29753]  {:unknown, [27787]}
[29754]  {:unknown, [27841]}
[29755]  {:unknown, [27877]}
[29756]  {:unknown, [27877]}
[29757]  {:unknown, [184]}
[29758]  {:unknown, [32554]}
[29759]  {:unknown, [32540]}
[29760]  {:unknown, [32518]}
[29761]  {:unknown, [32595]}
[29762]  {:unknown, [32532]}
[29763]  {:unknown, [32530]}
[29764]  {:unknown, [32521]}
[29765]  {:unknown, [32534]}
[29766]  {:unknown, [32595]}
[29767]  {:unknown, [32538]}
[29768]  {:unknown, [32541]}
[29769]  {:unknown, [32519]}
[29770]  {:unknown, [32540]}
[29771]  {:unknown, [32595]}
[29772]  {:unknown, [32519]}
[29773]  {:unknown, [32539]}
[29774]  {:unknown, [32534]}
[29775]  {:unknown, [32595]}
[29776]  {:unknown, [32542]}
[29777]  {:unknown, [32538]}
[29778]  {:unknown, [32513]}
[29779]  {:unknown, [32513]}
[29780]  {:unknown, [32540]}
[29781]  {:unknown, [32513]}
[29782]  {:unknown, [32607]}
[29783]  {:unknown, [32595]}
[29784]  {:unknown, [32530]}
[29785]  {:unknown, [32541]}
[29786]  {:unknown, [32535]}
[29787]  {:unknown, [32595]}
[29788]  {:unknown, [32522]}
[29789]  {:unknown, [32540]}
[29790]  {:unknown, [32518]}
[29791]  {:unknown, [32595]}
[29792]  {:unknown, [32512]}
[29793]  {:unknown, [32534]}
[29794]  {:unknown, [32534]}
[29795]  {:unknown, [32595]}
[29796]  {:unknown, [32522]}
[29797]  {:unknown, [32540]}
[29798]  {:unknown, [32518]}
[29799]  {:unknown, [32513]}
[29800]  {:unknown, [32512]}
[29801]  {:unknown, [32534]}
[29802]  {:unknown, [32543]}
[29803]  {:unknown, [32533]}
[29804]  {:unknown, [32595]}
[29805]  {:unknown, [32532]}
[29806]  {:unknown, [32530]}
[29807]  {:unknown, [32521]}
[29808]  {:unknown, [32538]}
[29809]  {:unknown, [32541]}
[29810]  {:unknown, [32532]}
[29811]  {:unknown, [32595]}
[29812]  {:unknown, [32529]}
[29813]  {:unknown, [32530]}
[29814]  {:unknown, [32528]}
[29815]  {:unknown, [32536]}
[29816]  {:unknown, [32605]}
[29817]  {:unknown, [32595]}
[29818]  {:unknown, [32595]}
[29819]  {:unknown, [32561]}
[29820]  {:unknown, [32518]}
[29821]  {:unknown, [32519]}
[29822]  {:unknown, [32595]}
[29823]  {:unknown, [32516]}
[29824]  {:unknown, [32530]}
[29825]  {:unknown, [32538]}
[29826]  {:unknown, [32519]}
[29827]  {:unknown, [32594]}
[29828]  {:unknown, [32595]}
[29829]  {:unknown, [32595]}
[29830]  {:unknown, [32570]}
[29831]  {:unknown, [32519]}
[29832]  {:unknown, [32595]}
[29833]  {:unknown, [32543]}
[29834]  {:unknown, [32540]}
[29835]  {:unknown, [32540]}
[29836]  {:unknown, [32536]}
[29837]  {:unknown, [32512]}
[29838]  {:unknown, [32595]}
[29839]  {:unknown, [32543]}
[29840]  {:unknown, [32538]}
[29841]  {:unknown, [32536]}
[29842]  {:unknown, [32534]}
[29843]  {:unknown, [32595]}
[29844]  {:unknown, [32512]}
[29845]  {:unknown, [32540]}
[29846]  {:unknown, [32542]}
[29847]  {:unknown, [32534]}
[29848]  {:unknown, [32540]}
[29849]  {:unknown, [32541]}
[29850]  {:unknown, [32534]}
[29851]  {:unknown, [32595]}
[29852]  {:unknown, [32516]}
[29853]  {:unknown, [32513]}
[29854]  {:unknown, [32540]}
[29855]  {:unknown, [32519]}
[29856]  {:unknown, [32534]}
[29857]  {:unknown, [32595]}
[29858]  {:unknown, [32540]}
[29859]  {:unknown, [32541]}
[29860]  {:unknown, [32595]}
[29861]  {:unknown, [32522]}
[29862]  {:unknown, [32540]}
[29863]  {:unknown, [32518]}
[29864]  {:unknown, [32513]}
[29865]  {:unknown, [32595]}
[29866]  {:unknown, [32533]}
[29867]  {:unknown, [32530]}
[29868]  {:unknown, [32528]}
[29869]  {:unknown, [32534]}
[29870]  {:unknown, [32595]}
[29871]  {:unknown, [32516]}
[29872]  {:unknown, [32539]}
[29873]  {:unknown, [32538]}
[29874]  {:unknown, [32543]}
[29875]  {:unknown, [32534]}
[29876]  {:unknown, [32595]}
[29877]  {:unknown, [32522]}
[29878]  {:unknown, [32540]}
[29879]  {:unknown, [32518]}
[29880]  {:unknown, [32595]}
[29881]  {:unknown, [32516]}
[29882]  {:unknown, [32534]}
[29883]  {:unknown, [32513]}
[29884]  {:unknown, [32534]}
[29885]  {:unknown, [32595]}
[29886]  {:unknown, [32518]}
[29887]  {:unknown, [32541]}
[29888]  {:unknown, [32528]}
[29889]  {:unknown, [32540]}
[29890]  {:unknown, [32541]}
[29891]  {:unknown, [32512]}
[29892]  {:unknown, [32528]}
[29893]  {:unknown, [32538]}
[29894]  {:unknown, [32540]}
[29895]  {:unknown, [32518]}
[29896]  {:unknown, [32512]}
[29897]  {:unknown, [32595]}
[29898]  {:unknown, [32540]}
[29899]  {:unknown, [32541]}
[29900]  {:unknown, [32595]}
[29901]  {:unknown, [32519]}
[29902]  {:unknown, [32539]}
[29903]  {:unknown, [32534]}
[29904]  {:unknown, [32595]}
[29905]  {:unknown, [32529]}
[29906]  {:unknown, [32534]}
[29907]  {:unknown, [32530]}
[29908]  {:unknown, [32528]}
[29909]  {:unknown, [32539]}
[29910]  {:unknown, [32594]}
[29911]  {:unknown, [32595]}
[29912]  {:unknown, [32595]}
[29913]  {:unknown, [32551]}
[29914]  {:unknown, [32539]}
[29915]  {:unknown, [32513]}
[29916]  {:unknown, [32540]}
[29917]  {:unknown, [32518]}
[29918]  {:unknown, [32532]}
[29919]  {:unknown, [32539]}
[29920]  {:unknown, [32595]}
[29921]  {:unknown, [32519]}
[29922]  {:unknown, [32539]}
[29923]  {:unknown, [32534]}
[29924]  {:unknown, [32595]}
[29925]  {:unknown, [32542]}
[29926]  {:unknown, [32538]}
[29927]  {:unknown, [32513]}
[29928]  {:unknown, [32513]}
[29929]  {:unknown, [32540]}
[29930]  {:unknown, [32513]}
[29931]  {:unknown, [32607]}
[29932]  {:unknown, [32595]}
[29933]  {:unknown, [32522]}
[29934]  {:unknown, [32540]}
[29935]  {:unknown, [32518]}
[29936]  {:unknown, [32595]}
[29937]  {:unknown, [32512]}
[29938]  {:unknown, [32534]}
[29939]  {:unknown, [32534]}
[29940]  {:unknown, [32595]}
[29941]  {:unknown, [32593]}
[29942]  {:pop, {:value, 2227}}
[29944]  {:unknown, [20988]}
[29945]  {:unknown, [12639]}
[29946]  {:unknown, 'g'}
[29947]  {:unknown, [19280]}
[29948]  {:unknown, [19282]}
[29949]  {:unknown, [19201]}
[29950]  {:unknown, [19217]}
[29951]  {:unknown, [19200]}
[29952]  {:unknown, [19219]}
[29953]  {:unknown, [19205]}
[29954]  {:unknown, [19230]}
[29955]  {:unknown, [19223]}
[29956]  {:unknown, [19222]}
[29957]  {:unknown, [19282]}
[29958]  {:unknown, [19227]}
[29959]  {:unknown, [19228]}
[29960]  {:unknown, [19282]}
[29961]  {:unknown, [19217]}
[29962]  {:unknown, [19226]}
[29963]  {:unknown, [19219]}
[29964]  {:unknown, [19200]}
[29965]  {:unknown, [19217]}
[29966]  {:unknown, [19229]}
[29967]  {:unknown, [19219]}
[29968]  {:unknown, [19230]}
[29969]  {:unknown, [19282]}
[29970]  {:unknown, [19229]}
[29971]  {:unknown, [19228]}
[29972]  {:unknown, [19282]}
[29973]  {:unknown, [19211]}
[29974]  {:unknown, [19229]}
[29975]  {:unknown, [19207]}
[29976]  {:unknown, [19200]}
[29977]  {:unknown, [19282]}
[29978]  {:unknown, [19220]}
[29979]  {:unknown, [19229]}
[29980]  {:unknown, [19200]}
[29981]  {:unknown, [19223]}
[29982]  {:unknown, [19226]}
[29983]  {:unknown, [19223]}
[29984]  {:unknown, [19219]}
[29985]  {:unknown, [19222]}
[29986]  {:unknown, [19292]}
[29987]  {:unknown, [19320]}
[29988]  {:unknown, [19320]}
[29989]  {:unknown, [19249]}
[29990]  {:unknown, [19229]}
[29991]  {:unknown, [19228]}
[29992]  {:unknown, [19221]}
[29993]  {:unknown, [19200]}
[29994]  {:unknown, [19219]}
[29995]  {:unknown, [19206]}
[29996]  {:unknown, [19207]}
[29997]  {:unknown, [19230]}
[29998]  {:unknown, [19219]}
[29999]  {:unknown, [19206]}
[30000]  {:unknown, [19227]}
[30001]  {:unknown, [19229]}
[30002]  {:unknown, [19228]}
[30003]  {:unknown, [19201]}
[30004]  {:unknown, [19273]}
[30005]  {:unknown, [19282]}
[30006]  {:unknown, [19211]}
[30007]  {:unknown, [19229]}
[30008]  {:unknown, [19207]}
[30009]  {:unknown, [19282]}
[30010]  {:unknown, [19226]}
[30011]  {:unknown, [19219]}
[30012]  {:unknown, [19204]}
[30013]  {:unknown, [19223]}
[30014]  {:unknown, [19282]}
[30015]  {:unknown, [19200]}
[30016]  {:unknown, [19223]}
[30017]  {:unknown, [19219]}
[30018]  {:unknown, [19217]}
[30019]  {:unknown, [19226]}
[30020]  {:unknown, [19223]}
[30021]  {:unknown, [19222]}
[30022]  {:unknown, [19282]}
[30023]  {:unknown, [19206]}
[30024]  {:unknown, [19226]}
[30025]  {:unknown, [19223]}
[30026]  {:unknown, [19282]}
[30027]  {:unknown, [19223]}
[30028]  {:unknown, [19228]}
[30029]  {:unknown, [19222]}
[30030]  {:unknown, [19282]}
[30031]  {:unknown, [19229]}
[30032]  {:unknown, [19220]}
[30033]  {:unknown, [19282]}
[30034]  {:unknown, [19206]}
[30035]  {:unknown, [19226]}
[30036]  {:unknown, [19223]}
[30037]  {:unknown, [19282]}
[30038]  {:unknown, [19217]}
[30039]  {:unknown, [19226]}
[30040]  {:unknown, [19219]}
[30041]  {:unknown, [19230]}
[30042]  {:unknown, [19230]}
[30043]  {:unknown, [19223]}
[30044]  {:unknown, [19228]}
[30045]  {:unknown, [19221]}
[30046]  {:unknown, [19223]}
[30047]  {:unknown, [19283]}
[30048]  {:unknown, [19320]}
[30049]  {:unknown, [19320]}
