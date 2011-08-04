
;; Depends on the two preceding exercises

;; Let x, y and z be intervals. We will use the following prefixes with them:

;; l - lower-bound (e.g. lx is the lower-bound of x)
;; u - upper-bound
;; c - center of interval
;; w - width of interval (distance from center to either bound)
;; p - percentage tolerance 
;;                             w                    u - l       u + l
;; We need these formulas: p = ─  =>  w = p⋅c,  w = ─────,  c = ───── 
;;                             c                      2           2
;;                     u - l
;; It follows that p = ─────,  l = c - w  and  u = c + w
;;                     u + l
;;
;; Let's construct a multiplication z = x⋅y. Assuming x > 0 and y > 0 
;; (see 2.11), and expanding x, y and z to interval notation, we see that
;; z = x⋅y  =>  [lz, uz] = [lx, ux]⋅[ly, uy] = [lx⋅ly, ux⋅uy], so
;; lz = lx⋅ly and uz = ux⋅uy.
;;                                              uz - lz   ux⋅uy - lx⋅ly
;; What is pz in terms of px and py? Well, pz = ─────── = ─────────────.
;;                                              uz + lz   ux⋅uy + lx⋅ly
;;
;; We shall express lx, ly, ux and uy in terms of cx, cy, px and py.
;; Using the formulas, we get:
;;
;; lx = cx - wx = cx - px⋅cx = cx(1 - px)
;; ly = cy - wy = cy - py⋅cy = cy(1 - py)
;; ux = cx + wx = cx + px⋅cx = cx(1 + px)
;; uy = cy + wy = cy + py⋅cy = cy(1 + py), and from these:
;;
;; lx⋅ly = cx⋅cy(1 - px)(1 - py)
;; ux⋅uy = cx⋅cy(1 + px)(1 + py).
;;                                                           ┌           ┐
;;           cx⋅cy(1 + px)(1 + py) - cx⋅cy(1 - px)(1 - py)   │ factoring │
;; Now, pz = ───────────────────────────────────────────── = │ out cx⋅cy │ =
;;           cx⋅cy(1 + px)(1 + py) + cx⋅cy(1 - px)(1 - py)   └           ┘  
;;                                         ┌                 ┐ 
;;   (1 + px)(1 + py) - (1 - px)(1 - py)   │ after expanding │    px + py
;; = ─────────────────────────────────── = │       and       │ = ─────────.
;;   (1 + px)(1 + py) + (1 - px)(1 - py)   │   simplifying   │   1 + px⋅py
;;                                         └                 ┘
;; Assuming that px⋅py << 1, pz ≈ px + py. Test confirms our result:

(define res1 (make-center-percent 4.7 0.02))
(define res2 (make-center-percent 3.3 0.03))

(define res1*res2 (mul-interval res1 res2))

(percent res1)      ; 0.02
(percent res2)      ; 0.03
(percent res1*res2) ; 0.04997
