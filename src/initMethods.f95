module nt_InitMethodsModule

    public :: randomInit, constInit

    contains

        subroutine randomInit(weight, args)
            real, intent(out) :: weight
            real, intent(in) :: args(0:)
            call random_number(weight)
        end subroutine randomInit

        subroutine fixedInit(weight, args)
            real, intent(out) :: weight
            real, intent(in) :: args(0:)

            weight = args(0)
        end subroutine fixedInit

end module nt_InitMethodsModule

