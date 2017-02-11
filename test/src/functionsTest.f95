program functionsTest
    use nt_TypesModule
    use nt_NeuronModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)
    
    call sft_summary(testSuite)


end program functionsTest
