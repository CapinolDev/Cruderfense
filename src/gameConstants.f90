module gameconstants
    use iso_c_binding
    implicit none
    
    enum, bind(c)
        enumerator :: SCREEN_LOGO = 0
        enumerator :: SCREEN_TITLE
        enumerator :: SCREEN_GAMEPLAY
        enumerator :: SCREEN_ENDING
    end enum
end module gameconstants