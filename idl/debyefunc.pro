function debyefunc, x
;_Titl  DEBYEFUNC  Debye Function to be called for integration
; x      in. Independent variable
; func. out. Precision set by input type
;_Hist 2010feb11 Hugh Kieffer  from Kittel p 137 (32)
;_end
ex=exp(x)
out=x^4 *ex/(ex-1.)^2
return,out
end
