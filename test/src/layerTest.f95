program layerTest
    use nt_TypesModule
    use nt_LayerModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)

    call sft_run(testSuite, shouldCorrectlyInitInputLayer)
    !call sft_run(testSuite, shouldCorrectlyInitOutputLayer)
    !call sft_run(testSuite, shouldCorrectlyInitHiddenLayer)
    
    call sft_summary(testSuite)
    
    contains 
        function shouldCorrectlyInitInputLayer() result(res)
            logical :: res
            !given
            type(nt_Layer) :: layer
            
            !when
            call nt_layerInit(layer, 0, 5, 10, 0)

            !then
            res = sft_assertEqual(layer%layerSize, 5) &
                .and. sft_assertEqual(layer%id, 0) &
                .and. sft_assertEqual(layer%neurons(0)%nextLayerSize, 10)

        end function shouldCorrectlyInitInputLayer

        function shouldCorrectlyInitOutputLayer() result(res)
            logical :: res
            type(nt_Layer) :: layer

            res = .TRUE.
        end function shouldCorrectlyInitOutputLayer

        function shouldCorrectlyInitHiddenLayer() result(res)
            logical :: res
            type(nt_Layer) :: layer
            
            res = .TRUE.
        end function shouldCorrectlyInitHiddenLayer

end program layerTest
