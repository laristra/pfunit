
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
     
   contains
!     procedure :: verifyAgainst
     procedure :: verify_ => verify_ArgumentsEqual
     procedure :: verify_i1_ => verify_i1_ArgumentsEqual
     procedure :: argumentsToBeVerified => argumentsToBeVerified_
  end type ArgumentsEqual

!!!! Or do we invoke the predicate from the subject?

  interface ArgumentsEqual
     module procedure newArgumentsEqual
  end interface ArgumentsEqual

  interface verify
     module procedure verify_ArgumentsEqual
     module procedure verify_i1_ArgumentsEqual
  end interface verify

contains

  type(ArgumentsEqual) function newArgumentsEqual(i1) result(pred_)
    integer, intent(in) :: i1
    pred_%name = 'argumentsEqual'
    pred_%i1   = i1
    !print *,19000,pred_%name    
  end function newArgumentsEqual

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

  character(len=MAXLEN_MESSAGE) function valuesReport(expected, found)
    use StringConversionUtilities_mod, only: toString
    integer, intent(in) :: expected, found
    valuesReport = 'expected: <'//trim(toString(expected))//'> but found: <'&
         &//trim(toString(found))//'>'
  end function valuesReport

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


