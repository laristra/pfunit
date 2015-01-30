!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: PredicateArgumentsEqual
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Mike Rilee, Rilee Systems Technologies
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


module PredicateArgumentsEqual_mod
  use Event_mod
  use EventPolyWrap_mod
  use EventPolyWrapVector_mod
  use Exception_mod
  use Predicate_mod

  implicit none
  private

  public :: ArgumentsEqual
!  public :: ArgumentsEqual, nArgumentsEqual

  type, extends(Predicate) :: ArgumentsEqual

!!! Data for verification
     integer :: i1
     real :: r1
     
   contains
!     procedure :: verifyAgainst
     procedure :: verify_ => verify_ArgumentsEqual
     procedure :: verify_i1_ArgumentsEqual
     procedure :: verify_r1_ArgumentsEqual
     generic :: verify_1 => verify_i1_ArgumentsEqual, verify_r1_ArgumentsEqual
     procedure :: verify_i1_ => verify_i1_ArgumentsEqual
     procedure :: verify_r1_ => verify_r1_ArgumentsEqual
     procedure :: verify_p1_ => verify_p1_ArgumentsEqual
     procedure :: argumentsToBeVerified => argumentsToBeVerified_
  end type ArgumentsEqual

!!!! Or do we invoke the predicate from the subject?

  interface ArgumentsEqual
     module procedure newArgumentsEqual_i
     module procedure newArgumentsEqual_r
  end interface ArgumentsEqual

  interface verify
     module procedure verify_ArgumentsEqual
     !module procedure verify_i1_ArgumentsEqual
     module procedure verify_p1_ArgumentsEqual
  end interface verify

  interface verify_1a
     module procedure :: verify_i1_ArgumentsEqual
     module procedure :: verify_r1_ArgumentsEqual
  end interface verify_1a

contains

  type(ArgumentsEqual) function newArgumentsEqual_i(i1) result(pred_)
    integer, intent(in) :: i1
    pred_%name = 'argumentsEqual'
    pred_%i1   = i1
    !print *,19000,pred_%name    
  end function newArgumentsEqual_i

  type(ArgumentsEqual) function newArgumentsEqual_r(r1) result(pred_)
    real, intent(in) :: r1
    pred_%name = 'argumentsEqual'
    pred_%r1   = r1
    !print *,19000,pred_%name    
  end function newArgumentsEqual_r

  ! Enable argument checking.
  logical function argumentsToBeVerified_(this)
    class(ArgumentsEqual), intent(inout) :: this
    !print *,20001,this%name    
    argumentsToBeVerified_ = .true.
  end function argumentsToBeVerified_

! This one is here only to satisfy the deferred requirement from Predicate.F90.
  
  logical function verify_ArgumentsEqual(this,subj,eventList) result(verifiedp_)
    class (ArgumentsEqual), intent(inout) :: this
    character(*), intent(in) :: subj
    type (EventPolyWrapVector), intent(in) :: eventList
    
    class (Event), pointer :: event_
    type (EventPolyWrap), pointer :: eventContainer
    type (EventPolyWrapVectorIterator) :: iter
    logical :: ok
    integer :: nCalls

    ! verify_ArgumentsEqual doesn't have anything to say if it's called without
    ! arguments.
    verifiedp_ = .true.

  end function verify_ArgumentsEqual


  logical function verify_i1_ArgumentsEqual(this,subj,eventList&
       & ,i1 )
    use StringConversionUtilities_mod, only: toString
    class (ArgumentsEqual), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    integer, intent(in) :: i1
    !call throw('verify_i1_::not implemented')
    !verify_i1_ArgumentsEqual = .false.
    if (this%i1 == i1) then
       verify_i1_ArgumentsEqual = .true.
    else
       verify_i1_ArgumentsEqual = .false.
       call throw( & ! '             "'// &
            & trim(subj)//'" "'//trim(this%name)//'" does not hold: ' &
            & // valuesReport(this%i1,i1))
    end if
       
  end function verify_i1_ArgumentsEqual

  ! Note:  No tolerance for the real case below.
  
  ! Note: that real and integer of the above and below are essentially
  ! identical. Can we put these into one, maybe using polymorphism?
  logical function verify_r1_ArgumentsEqual(this,subj,eventList&
       & ,r1 )
    use StringConversionUtilities_mod, only: toString
    class (ArgumentsEqual), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    real, intent(in) :: r1
    !call throw('verify_r1_::not implemented')
    !verify_r1_ArgumentsEqual = .false.
    if (this%r1 == r1) then
       verify_r1_ArgumentsEqual = .true.
    else
       verify_r1_ArgumentsEqual = .false.
       call throw( & ! '             "'// &
            & trim(subj)//'" "'//trim(this%name)//'" does not hold: ' &
            & // valuesReport_rr(this%r1,r1))
    end if
       
  end function verify_r1_ArgumentsEqual

  logical function verify_p1_ArgumentsEqual(this,subj,eventList&
       &, p1 )
    class (ArgumentsEqual), intent(inout) :: this
    character(*), intent(in) :: subj
    type(EventPolyWrapVector), intent(in) :: eventList
    class(*), intent(in) :: p1

    !call throw('verify_p1_::not implemented')
!    verify_p1_ArgumentsEqual = .false.

!fail    verify_p1_ArgumentsEqual = verify_1a(this,subj,eventList,p1)

    ! Is there a better way?  Maybe a visitor pattern?
    
    select type(p1)
    type is (integer)
       !verify_p1_ArgumentsEqual = this%verify_1(subj,eventList,p1)
       verify_p1_ArgumentsEqual = verify_1a(this,subj,eventList,p1)
       return
    type is (real)
       !verify_p1_ArgumentsEqual = this%verify_1(subj,eventList,p1)
       verify_p1_ArgumentsEqual = verify_1a(this,subj,eventList,p1)
       return
    end select
    
    call throw('verify_p1_::dispatch on type not implemented')
    verify_p1_ArgumentsEqual = .false.
    
  end function verify_p1_ArgumentsEqual

  ! Can we put the two below into a generic interface?
  character(len=MAXLEN_MESSAGE) function valuesReport(expected, found)
    use StringConversionUtilities_mod, only: toString
    integer, intent(in) :: expected, found
    valuesReport = 'expected: <'//trim(toString(expected))//'> but found: <'&
         &//trim(toString(found))//'>'
  end function valuesReport

  character(len=MAXLEN_MESSAGE) function valuesReport_rr(expected, found)
    use StringConversionUtilities_mod, only: toString
    real, intent(in) :: expected, found
    valuesReport_rr = 'expected: <'//trim(toString(expected))//'> but found: <'&
         &//trim(toString(found))//'>'
  end function valuesReport_rr

end module PredicateArgumentsEqual_mod

! Not English...
! call subject%verify(argumentsEqual)

! in verifier
! next subject, next exp -> argumentsEqual
! call this%verify(subject,argumentsEqual)
! sub verify -> argumentsEqual%verifyAgainst(subject)

! or in Verifier::verify
! if argumentsEqual%verifyAgainst(subj,exp) then...
!


