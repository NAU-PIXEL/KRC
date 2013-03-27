
	INTEGER FUNCTION BIGEND ()
C_Title  Determine if hardware we are on is big or little endian
C_Arguments
C_Calls 0
C_History  20130130  Randy Kaelber original version
C_end
C

        BIGEND = 0
        if (ichar(transfer(1,'a')) == 0) then
            BIGEND = 1
        endif

        RETURN
	END
