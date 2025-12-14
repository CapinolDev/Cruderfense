program main
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: raylib
    implicit none (type, external)


    type(color_type) :: bgColor, playerColor 


    integer, parameter :: SCREEN_WIDTH  = 1920
    integer, parameter :: SCREEN_HEIGHT = 1080
    


    
    integer, parameter :: CHAR_SIZE = 100
    integer :: char_x, char_y

    integer, parameter :: MOVE_SPEED = 10


    bgColor = color_from_hsv(210.0, 10.0, 97.0)
    playerColor = LIGHTGRAY
    


    

    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Cruderfense' // c_null_char)

    
    
    
    
    char_x = SCREEN_WIDTH / 2 - CHAR_SIZE / 2
    char_y = SCREEN_HEIGHT / 2 - CHAR_SIZE / 2
    call set_target_fps(60)

    do while (.not. window_should_close())
        call handleInput
        call begin_drawing()
            call clear_background(bgColor)
            call draw_rectangle(char_x, char_y, CHAR_SIZE, CHAR_SIZE, playerColor)
        call end_drawing()
    end do

    call close_window()

    contains

    subroutine handleInput()

        if (is_key_down(KEY_D) .or. is_key_down(KEY_RIGHT)) then
            char_x = char_x + MOVE_SPEED
        end if
        
        
        if (is_key_down(KEY_A) .or. is_key_down(KEY_LEFT)) then
            char_x = char_x - MOVE_SPEED
        end if
        
        
        if (is_key_down(KEY_W) .or. is_key_down(KEY_UP)) then
            char_y = char_y - MOVE_SPEED
        end if
        
        
        if (is_key_down(KEY_S) .or. is_key_down(KEY_DOWN)) then
            char_y = char_y + MOVE_SPEED
        end if

        char_x = max(0, min(char_x, SCREEN_WIDTH - CHAR_SIZE))
        char_y = max(0, min(char_y, SCREEN_HEIGHT - CHAR_SIZE))

    end subroutine
end program main