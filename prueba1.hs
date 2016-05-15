import Asistente_pruebas

verify1 = let theorem = (p <==> q) <==> q === p in
         proof theorem
         >>=
         statement 3.1 with (q =: r) using lambda z (z)
         >>=
         statement 3.3 with (q =: p) using lambda z (p <==> z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem

verify2 = let theorem = (p <==> p) <==> (q <==> q) === true in
         proof theorem
         >>=
         statement 3.3 with (p =: p) using lambda z (z <==> (q <==> q))
         >>=
         statement 3.3 with (q =: p) using lambda z (true <==> z)
         >>=
         statement 3.3 with (true =: p) using lambda z (z)
         >>=
         done theorem

verify3 = let theorem = (p <==> p) <==> true === true in
         proof theorem
         >>=
         statement 3.3 with (p =: p) using lambda z (z <==> true)
         >>=
         statement 3.3 with (true =: p) using lambda z (z)
         >>=
         done theorem

verify4 = let theorem = (p <==> q) <==> (q <==> p) === true in
         proof theorem
         >>=
         statement 3.2 with (p =: p) using lambda z ((p <==> q) <==> z)
         >>=
         statement 3.3 with (p <==> q =: p) using lambda z (z)
         >>=
         done theorem

verify5 = let theorem = p === (q <==> q) <==> p in
         proof theorem
         >>=
         statement 3.5 with (p =: p) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (z <==> q)
         >>=
         statement 3.1 with (q, p, q =: p, q, r) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (q <==> z)
         >>=
         statement 3.1 with (q, p =: p, r) using lambda z (z)
         >>=
         done theorem

