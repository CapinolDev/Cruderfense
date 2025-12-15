program main
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: raylib
    implicit none (type, external)


    type(color_type) :: bgColor, playerColor 


    integer, parameter :: SCREEN_WIDTH  = 1920
    integer, parameter :: SCREEN_HEIGHT = 1080
    
    integer :: mouseX, mouseY
    


    type :: EnemyChar 
        character(len=30) :: name
        integer :: xPos, yPos
        integer :: maxHp, hp
        real :: size
        type(color_type) :: charCol
    end type EnemyChar
    
    type :: playerChar
        character(len=30) :: name
        integer :: xPos, yPos
        integer :: maxHp, hp
        integer :: size
        type(color_type) :: charCol
        integer :: moveSpeed = 10
    end type playerChar

    integer, parameter :: CHAR_SIZE = 100
    integer :: char_x, char_y
    



    type(EnemyChar) :: Enemy1
    type(playerChar) :: player1

    bgColor = color_from_hsv(210.0, 10.0, 97.0)
    playerColor = LIGHTGRAY
    
    

    Enemy1%name = "Diddy"
    Enemy1%xPos = 20
    Enemy1%yPos = 30
    Enemy1%size = 30.0 
    Enemy1%maxHp = 10
    Enemy1%hp = 10
    Enemy1%charCol = color_from_hsv(150.1,10.2,97.0)
    
    player1%name = "Adam_goon"
    player1%xPos = 20
    player1%yPos = 30
    player1%size = 70
    player1%maxHp = 10
    player1%hp = 10
    player1%charCol = color_from_hsv(152.1,10.2,97.0)
    

    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Cruderfense' // c_null_char)

    
    call disable_cursor()
    
    
    player1%xPos = SCREEN_WIDTH / 2 - CHAR_SIZE / 2
    player1%yPos= SCREEN_HEIGHT / 2 - CHAR_SIZE / 2
    call set_target_fps(60)

    do while (.not. window_should_close())
        call handleInput(player1)
        call mouseHandling
        call begin_drawing()
            call clear_background(bgColor)
            call drawChar(player1)
            call draw_circle(mouseX,mouseY,15.0,WHITE)
            call spawnEnemy(Enemy1)
        call end_drawing()
    end do

    call close_window()

    contains

    subroutine handleInput(player)
        type(playerChar) :: player

        if (is_key_down(KEY_D) .or. is_key_down(KEY_RIGHT)) then
            player%xPos = player%xPos + player%moveSpeed
        end if
        
        
        if (is_key_down(KEY_A) .or. is_key_down(KEY_LEFT)) then
            player%xPos = player%xPos - player%moveSpeed
        end if
        
        
        if (is_key_down(KEY_W) .or. is_key_down(KEY_UP)) then
            player%yPos = player%yPos - player%moveSpeed
        end if
        
        
        if (is_key_down(KEY_S) .or. is_key_down(KEY_DOWN)) then
            player%yPos = player%yPos + player%moveSpeed
        end if

        player%xPos = max(0, min(player%xPos, SCREEN_WIDTH - player%size))
        player%yPos = max(0, min(player%yPos, SCREEN_HEIGHT - player%size))

    end subroutine

    subroutine mouseHandling()

        
       mouseX = get_mouse_x()
       mouseY = get_mouse_y()

    end subroutine

    subroutine spawnEnemy(enemy)
        type(EnemyChar) :: enemy 

        call draw_circle(enemy%xPos,enemy%yPos,enemy%size,enemy%charCol)
        
    end subroutine

    subroutine drawChar(player)
        type(playerChar) :: player
        call draw_rectangle(player%xPos, player%yPos, player%size, player%size, player%charCol)
        call draw_text( player%name// c_null_char, (player%xPos)-10, (player%yPos)-20, 30, BLACK)
    end subroutine

end program main