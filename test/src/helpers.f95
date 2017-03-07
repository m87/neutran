module testHelpers

    public :: testWeightInit, testActivationFunction

    contains

        subroutine testWeightInit(weight, args)
            real, intent(out) :: weight
            real, intent(in) :: args(0:)

            weight = args(0)
        end subroutine testWeightInit

        function testActivationFunction(x, args) result(fx)
            real, intent(in) :: args(0:)
            real, intent(in) :: x
            real :: fx

            fx = x / args(0)

        end function testActivationFunction



end module testHelpers
