program main
    use :: collisions
    use :: gameconstants
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: raylib
    implicit none (type, external)


    type(color_type) :: bgColor, playerColor 
    

    integer, parameter :: SCREEN_WIDTH  = 1920
    integer, parameter :: SCREEN_HEIGHT = 1080
    
    real :: logoTrans = 0.0, logoTime = 0.0

    integer :: mouseX, mouseY
    real :: invisTicks
    type(texture2d_type) :: mainLogo
    logical :: isDead =.false.

    type :: Button
        type(rectangle_type) :: rect
        type(color_type) :: col
        logical :: isPressed
    end type Button

    type :: EnemyChar 
        character(len=30) :: name
        integer :: xPos, yPos
        integer :: maxHp, hp
        integer :: speed
        real :: size
        type(color_type) :: charCol
    end type EnemyChar

    type(EnemyChar), allocatable :: enemyArr(:)
    
    type :: playerChar
        character(len=30) :: name
        integer :: xPos, yPos
        integer :: maxHp, hp
        integer :: size
        type(color_type) :: charCol
        integer :: moveSpeed = 10
    end type playerChar

    character(len=2) :: playerHpStr, playerMaxHpStr
 
    integer, parameter :: CHAR_SIZE = 100
    integer :: char_x, char_y
    
    type(Button) :: startBtn, menuBtn



    type(EnemyChar) :: Enemy1, Enemy2
    type(playerChar) :: player1


    integer :: currentScreen = SCREEN_TITLE


    startBtn%rect%x = 800
    startBtn%rect%y = 450
    startBtn%rect%width = 200
    startBtn%rect%height = 60

    menuBtn%rect%x = 800
    menuBtn%rect%y = 450
    menuBtn%rect%width = 200
    menuBtn%rect%height = 60


    bgColor = color_from_hsv(210.0, 10.0, 97.0)
    playerColor = LIGHTGRAY
    
    
    
    allocate(enemyArr(0))

    Enemy1%name = "Diddy"
    Enemy1%xPos = 20
    Enemy1%yPos = 30
    Enemy1%size = 30.0 
    Enemy1%maxHp = 10
    Enemy1%hp = 10
    Enemy1%charCol = color_from_hsv(150.1,10.2,97.0)
    Enemy1%speed = 2

    Enemy2%name = "Diddy2"
    Enemy2%xPos = 700
    Enemy2%yPos = 60
    Enemy2%size = 60.0 
    Enemy2%maxHp = 20
    Enemy2%hp = 20
    Enemy2%charCol = color_from_hsv(150.1,10.2,97.0)
    Enemy2%speed = 4
    
    player1%name = "Adam_goon"
    player1%xPos = 20
    player1%yPos = 30
    player1%size = 50
    player1%maxHp = 10
    player1%hp = 10
    player1%charCol = color_from_hsv(152.1,10.2,97.0)
    

    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Cruderfense' // c_null_char)

    
    call disable_cursor()
    

    mainLogo = load_texture('src/assets/CapinolLogo.png' // c_null_char)

    player1%xPos = SCREEN_WIDTH / 2 - CHAR_SIZE / 2
    player1%yPos= SCREEN_HEIGHT / 2 - CHAR_SIZE / 2
    call set_target_fps(60)

    call spawnEnemy(Enemy1, enemyArr)
    call spawnEnemy(Enemy2, enemyArr)
    
    do while (.not. window_should_close())
        select case(currentScreen)
            case(SCREEN_GAMEPLAY)    
                call handleInput(player1)
                call mouseHandling
                
                invisTicks = invisTicks + 1
                write(playerHpStr, '(I0)') player1%hp
                write(playerMaxHpStr, '(I0)') player1%maxHp
                if (player1%hp < 1) then 
                    currentScreen = SCREEN_ENDING
                    isDead = .true.
                end if
            case(SCREEN_TITLE)
                call mouseHandling
                if (UpdateButton(startBtn)) then
                    currentScreen = SCREEN_GAMEPLAY  
                end if
            case(SCREEN_ENDING)
                call mouseHandling
                if (UpdateButton(menuBtn)) then
                    currentScreen = SCREEN_TITLE
                    if (isDead .eqv. .true.) then 
                        isDead = .false.
                        call resetGame
                    end if
                end if 

        end select
        call begin_drawing()
            select case(currentScreen)
                case(SCREEN_LOGO)
                    if (logoTrans < 1.0) then 
                        logoTrans = logoTrans + 0.01
                    else 
                        logoTime = logoTime + 0.1
                    end if
                    
                    if (logoTime > 5.0) then 
                        currentScreen = SCREEN_TITLE
                        
                    end if
                    call clear_background(bgColor)
                    call draw_texture(mainLogo, 590, 140, fade(WHITE,logoTrans))
                    call draw_text( "Capinol"// c_null_char, 760, 760, 80, fade(BLACK,logoTrans))
                case(SCREEN_TITLE)
                    call clear_background(bgColor)
                    
                    call draw_rectangle_rec(startBtn%rect, startBtn%col)
                    call draw_text("START" // c_null_char, int(startBtn%rect%x) + 50, &
                    int(startBtn%rect%y) + 15, 30, DARKGRAY)
                    call draw_text("Crudefense" // c_null_char, 500, 100, 140, BLACK)
                    call draw_circle(mouseX,mouseY,15.0,WHITE)
                    
                case(SCREEN_GAMEPLAY) 
                    
                    call clear_background(bgColor)
                    call drawChar(player1)
                    
                    call moveEnemies(enemyArr, player1)
                    call checkEnemyColl(enemyArr, player1)
                    call drawEnemies(enemyArr)
                    call draw_circle(mouseX,mouseY,15.0,WHITE)
                    
                    call draw_text(playerHpStr // c_null_char, 100, 200, 140, BLACK)
                case(SCREEN_ENDING)
                    call clear_background(bgColor)
                    call draw_rectangle_rec(menuBtn%rect, menuBtn%col)
                    call draw_text("Menu" // c_null_char, int(startBtn%rect%x) + 50, &
                    int(startBtn%rect%y) + 15, 30, DARKGRAY)
                    call draw_circle(mouseX,mouseY,15.0,WHITE)
            end select
        call end_drawing()
    end do

    call close_window()

    contains

    subroutine resetGame()
        if (allocated(enemyArr)) deallocate(enemyArr)
        allocate(enemyArr(0))

        
        player1%hp = player1%maxHp
        player1%xPos = SCREEN_WIDTH / 2 - player1%size / 2
        player1%yPos = SCREEN_HEIGHT / 2 - player1%size / 2
        invisTicks = 61.0 

        
        call spawnEnemy(Enemy1, enemyArr)
        call spawnEnemy(Enemy2, enemyArr)
    end subroutine
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

    subroutine spawnEnemy(newEnemy, Arr)
        type(EnemyChar), intent(in) :: newEnemy
        type(EnemyChar), allocatable, intent(inout) :: Arr(:)
        type(EnemyChar), allocatable :: temp(:)
        integer :: n

        n = size(Arr)
        allocate(temp(n + 1))
        
        if (n > 0) temp(1:n) = Arr
        temp(n + 1) = newEnemy
        
        call move_alloc(from=temp, to=Arr)
    end subroutine

    subroutine drawEnemies(Arr)

        type(EnemyChar) ::  Arr(:)
        
        type(EnemyChar) :: enemy
        integer :: i = 1

        
        do i=1,size(Arr)    
            enemy = Arr(i)
            
            call draw_circle(enemy%xPos,enemy%yPos,enemy%size,enemy%charCol)
            call draw_text(enemy%name// c_null_char, (enemy%xPos)-10,(enemy%yPos)-20,30,BLACK)
        end do
        
    end subroutine

    subroutine drawChar(player)
        type(playerChar) :: player

        real :: cR 

        cR = player%size 
        call draw_circle(player%xPos, player%yPos, cR, player%charCol)

        call draw_text( player%name// c_null_char, (player%xPos)-10, (player%yPos)-20, 30, BLACK)
    end subroutine

    subroutine moveEnemies(Arr, playr)

        type(EnemyChar), target ::  Arr(:)
        type(EnemyChar), pointer :: enemy
        type(playerChar) :: playr

        integer :: i = 0

        do i=1,size(Arr)    
            enemy => Arr(i)
            if (enemy%xPos > playr%xPos) then 
                enemy%xPos = enemy%xPos - enemy%speed
                
            else 
                enemy%xPos = enemy%xPos + enemy%speed
                
            end if

            if (enemy%yPos > playr%yPos) then 
                enemy%yPos = enemy%yPos - enemy%speed
                
            else 
                enemy%yPos = enemy%yPos + enemy%speed
                
            end if
            
            
        end do


    end subroutine


    subroutine checkEnemyColl(Arr, playr)
        type(EnemyChar), intent(in) :: Arr(:)
        TYPE(playerChar), intent(inout) :: playr 

        type(EnemyChar) :: enemy
        integer :: i = 0

        logical :: resFunc
        integer :: dx, dy, radsum, radsumsq

        integer :: i1
        resFunc = .false.
        do i = 1,size(Arr)
            
            enemy = Arr(i)
            i1 = enemy%size
            resFunc = circToCircColl(enemy%xPos,enemy%yPos,playr%xPos,playr%yPos,i1,playr%size)
            
            if (resFunc .eqv. .true.) then
                call damagePlayer(invisTicks, playr, 1)
                
            end if

        end do

    end subroutine

    subroutine damagePlayer(iFrameTicks, playr, damage)
        real, intent(inout) :: iFrameTicks 
        type(playerChar), intent(inout) :: playr 
        integer, intent(in) :: damage

        if (iFrameTicks > 60) then 
            
            iFrameTicks = 0
            playr%hp = playr%hp - damage
        end if


    end subroutine

    function UpdateButton(btn) result(clicked)
        type(Button), intent(inout) :: btn
        logical :: clicked
        type(vector2_type) :: mousePos
        
        clicked = .false.
        mousePos = get_mouse_position()

        
        if (check_collision_point_rec(mousePos, btn%rect)) then
            btn%col = GRAY 
            
            if (is_mouse_button_pressed(MOUSE_BUTTON_LEFT)) then
                clicked = .true.
                btn%col = BLACK 
            end if
        else
            btn%col = LIGHTGRAY 
        end if
    end function
end program main