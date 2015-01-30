!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: EventPolyWrap
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 29 Jan 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 29 Jan 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------


module EventPolyWrap_mod
      use Event_mod, only: Event   
   implicit none
   private

   public EventPolyWrap

   type EventPolyWrap
      class(Event), allocatable :: item
   contains
      procedure :: get

#ifdef __INTEL_COMPILER
      procedure :: copy
      generic :: assignment(=) => copy
#endif

   end type EventPolyWrap

   interface EventPolyWrap
      module procedure new_copy
   end interface EventPolyWrap


contains


   function new_copy(item) result(container)
      type (EventPolyWrap) :: container
      class(Event), intent(in) :: item
      allocate(container%item, source=item)
   end function new_copy

   
   function get(this) result(item)
      class (EventPolyWrap), target, intent(in) :: this
      class(Event), pointer :: item

      item => this%item

   end function get


#ifdef __INTEL_COMPILER
   subroutine copy(a, b)
      class(EventPolyWrap), intent(out) :: a
      class(EventPolyWrap), intent(in) :: b

      allocate(a%item, source=b%item)
   end subroutine copy
#endif


end module EventPolyWrap_mod

   
