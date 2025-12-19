module collisions
  implicit none
  private

  public :: circToCircColl
contains
  
  function circToCircColl(pos1x,pos1y,pos2x,pos2y,rad1,rad2) result(res)
          integer, intent(in) :: pos1x,pos1y,pos2x,pos2y,rad1,rad2
          integer :: dx, dy, radsum,radsumsq
          logical :: res

          res = .false.

          dx = pos1x - pos2x
          dy = pos1y - pos2y

          radsum = rad1 + rad2
          radsumsq = radsum * radsum 


            if ((dx * dx)+(dy*dy) <= radsumsq) then 
                res = .true.
            end if
    end function


end module collisions
