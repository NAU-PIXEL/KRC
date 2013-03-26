function bytepad, ss,len
;_Titl  bytePAD   Createa Byte version of a string, padded with trailing blanks
; ss	in.	initial string
; len	in.	Output length
; func	out.	Byte array , starting with ss, padded with trailing blanks
;_Desc
; if input string is longer than  len, it will be truncated

q=byte(ss)
n=n_elements(q)
if n lt len then begin ; need to pad
    blank=byte(' ')             ; byte for the blank character
    out=replicate(blank,len)
    out[0:n-1]=q 
endif else out=q[0:len-1]

return,out
end
 
