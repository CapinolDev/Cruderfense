program main
    use :: collisions
    use :: gameconstants
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: raylib
    implicit none (type, external)


    type(color_type) :: bgColor, playerColor 
    

    integer, parameter :: SCREEN_WIDTH  = 1920
    integer, parameter :: SCREEN_HEIGHT = 1080
    
    real :: crosshairX, crosshairY

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

    type :: Projectile
        real :: x, y
        real :: speedX, speedY
        real :: targetX, targetY
        real :: size
        logical :: active = .false.
        integer :: damage
    end type Projectile

    type(Projectile), allocatable :: arrows(:)
    integer, parameter :: maxArrows = 50

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
        logical :: isCharging = .false.
        real :: chargeValue = 0.0      
        real :: maxCharge = 1.5
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
    
    call set_config_flags(FLAG_WINDOW_RESIZABLE)

    call init_window(SCREEN_WIDTH, SCREEN_HEIGHT, 'Cruderfense' // c_null_char)

    crosshairX = SCREEN_WIDTH / 2.0
    crosshairY = SCREEN_HEIGHT / 2.0
    
    call disable_cursor()
    

    mainLogo = load_texture('src/assets/CapinolLogo.png' // c_null_char)

    player1%xPos = SCREEN_WIDTH / 2 - CHAR_SIZE / 2
    player1%yPos= SCREEN_HEIGHT / 2 - CHAR_SIZE / 2
    call set_target_fps(60)

    call spawnEnemy(Enemy1, enemyArr)
    call spawnEnemy(Enemy2, enemyArr)
    
    allocate(arrows(maxArrows))

    do while (.not. window_should_close())
        select case(currentScreen)
            case(SCREEN_GAMEPLAY)    
                call handleInput(player1)
                call mouseHandlingGame(player1)
                call updateArrows()

                invisTicks = invisTicks + 1
                write(playerHpStr, '(I0)') player1%hp
                write(playerMaxHpStr, '(I0)') player1%maxHp
                if (player1%hp < 1) then 
                    currentScreen = SCREEN_ENDING
                    isDead = .true.
                end if
            case(SCREEN_TITLE)
                call mouseHandling
                if (UpdateButton(startBtn, mouseX, mouseY)) then
                    currentScreen = SCREEN_GAMEPLAY  
                end if
            case(SCREEN_ENDING)
                call mouseHandling
                if (UpdateButton(menuBtn, mouseX, mouseY)) then
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
                    call drawArrows()
                    
                    if (player1%isCharging) then
                        
                        call draw_circle_lines(mouseX, mouseY, 40.0, fade(GRAY, 0.5))
                        
                        
                        
                        call draw_circle_lines(mouseX, mouseY, 40.0 - (35.0 * player1%chargeValue), YELLOW)
                        
                        
                        call draw_circle(mouseX, mouseY, 2.0, RED)
                    end if

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

        if (is_mouse_button_down(MOUSE_BUTTON_LEFT)) then
            player1%isCharging = .true.
            
            
            player1%chargeValue = player1%chargeValue + (get_frame_time() / player1%maxCharge)
            
            
            if (player1%chargeValue > 1.0) player1%chargeValue = 1.0
        else
            
            if (player1%isCharging) then
                 call fireArrow(player, mouseX, mouseY)
                player1%isCharging = .false.
                player1%chargeValue = 0.0
            end if
        end if

        player%xPos = max(0, min(player%xPos, SCREEN_WIDTH - player%size))
        player%yPos = max(0, min(player%yPos, SCREEN_HEIGHT - player%size))

    end subroutine


    subroutine mouseHandling()
        type(vector2_type) :: delta
        
        
        delta = get_mouse_delta()
        
        
        crosshairX = crosshairX + delta%x
        crosshairY = crosshairY + delta%y
        
        
        crosshairX = max(0.0, min(crosshairX, real(SCREEN_WIDTH)))
        crosshairY = max(0.0, min(crosshairY, real(SCREEN_HEIGHT)))
        
        
        mouseX = int(crosshairX)
        mouseY = int(crosshairY)
    end subroutine

    subroutine mouseHandlingGame(player)
        type(playerChar), intent(in) :: player
        type(vector2_type) :: delta
        real :: dirX, dirY, length, maxReach
        
        delta = get_mouse_delta()
        crosshairX = crosshairX + delta%x
        crosshairY = crosshairY + delta%y
        
        
        dirX = crosshairX - player%xPos
        dirY = player%yPos - crosshairY 
        dirX = crosshairX - player%xPos
        dirY = crosshairY - player%yPos
        
        length = sqrt(dirX**2 + dirY**2)
        if (length > 0) then
            dirX = dirX / length
            dirY = dirY / length
        end if

        
        maxReach = 60.0 + (300.0 * player%chargeValue)
        
        
        mouseX = int(player%xPos + (dirX * maxReach))
        mouseY = int(player%yPos + (dirY * maxReach))
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

    function UpdateButton(btn, mX, mY) result(clicked)
        type(Button), intent(inout) :: btn
        integer, intent(in) :: mX, mY
        logical :: clicked
        type(vector2_type) :: mousePos
        
      clicked = .false.
    
        mousePos%x = real(mX)
        mousePos%y = real(mY)

        
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

    subroutine fireArrow(player, mX, mY)
        type(playerChar), intent(in) :: player
        integer, intent(in) :: mX, mY
        real :: dirX, dirY, length, projSpeed
        integer :: i

        do i = 1, maxArrows
            if (.not. arrows(i)%active) then
                arrows(i)%active = .true.
                arrows(i)%x = real(player%xPos)
                arrows(i)%y = real(player%yPos)
                
                
                arrows(i)%targetX = real(mX)
                arrows(i)%targetY = real(mY)
                
                arrows(i)%size = 5.0
                
               
                dirX = arrows(i)%targetX - arrows(i)%x
                dirY = arrows(i)%targetY - arrows(i)%y
                length = sqrt(dirX**2 + dirY**2)
                
               
                projSpeed = 10.0 + (25.0 * player%chargeValue)
                
                if (length > 0) then
                    arrows(i)%speedX = (dirX / length) * projSpeed
                    arrows(i)%speedY = (dirY / length) * projSpeed
                end if
                exit
            end if
        end do
    end subroutine

subroutine updateArrows()
        integer :: i
        real :: distToTarget
        do i = 1, maxArrows
            if (arrows(i)%active) then
                arrows(i)%x = arrows(i)%x + arrows(i)%speedX
                arrows(i)%y = arrows(i)%y + arrows(i)%speedY
                
                
                distToTarget = sqrt((arrows(i)%targetX - arrows(i)%x)**2 + &
                                    (arrows(i)%targetY - arrows(i)%y)**2)
                
                
                if (distToTarget < 15.0 .or. & 
                    arrows(i)%x < 0 .or. arrows(i)%x > SCREEN_WIDTH .or. &
                    arrows(i)%y < 0 .or. arrows(i)%y > SCREEN_HEIGHT) then
                    
                    arrows(i)%active = .false.
                end if
            end if
        end do
    end subroutine
    
    subroutine drawArrows()
        integer :: i
        do i = 1, maxArrows
            if (arrows(i)%active) then
                call draw_circle(int(arrows(i)%x), int(arrows(i)%y), arrows(i)%size, BLACK)
            end if
        end do
    end subroutine

end program main