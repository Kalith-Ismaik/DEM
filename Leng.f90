!Function Leng provides the length of the string without the spaces.

    function leng( str )
        
        character str*(*)

        do leng = len(str), 1, -1
           if ( str(leng:leng).ne.' ' ) return
        end do

        return

    end function leng