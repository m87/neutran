program layerTest
    use nt_TypesModule
    use nt_LayerModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)

    call sft_run(testSuite, shouldCorrectlyInitInputLayer)
    call sft_run(testSuite, shouldCorrectlyInitOutputLayer)
    call sft_run(testSuite, shouldCorrectlyInitHiddenLayer)
    
    call sft_summary(testSuite)
    
    contains 
        function shouldCorrectlyInitHiddenLayer() result(res)
            logical :: res
            !given
            type(nt_Layer) :: layer
            
            !when
            call nt_layerInit(layer, 1, 5, 10, 4)

            !then
            res = sft_assertEqual(5, layer%layerSize) &
                .and. sft_assertEqual(4, layer%id) &
                .and. sft_assertEqual(5, size(layer%neurons)) &
                .and. sft_assertEqual(10, layer%neurons(0)%nextLayerSize)

        end function shouldCorrectlyInitHiddenLayer

        function shouldCorrectlyInitOutputLayer() result(res)
            logical :: res
            !given
            type(nt_Layer) :: layer

            !when
            call nt_layerInit(layer, 2, 5, 10, 4)

            !then
            res = sft_assertEqual(5, layer%layerSize) &
                .and. sft_assertEqual(4, layer%id) &
                .and. sft_assertEqual(5, size(layer%neurons)) &
                .and. sft_assertEqual(0, layer%neurons(0)%nextLayerSize)

        end function shouldCorrectlyInitOutputLayer

        function shouldCorrectlyInitInputLayer() result(res)
            logical :: res
            !given
            type(nt_Layer) :: layer

            !when
            call nt_layerInit(layer, 0, 5, 10, 4)

            !then
            res = sft_assertEqual(5, layer%layerSize) &
                .and. sft_assertEqual(4, layer%id) &
                .and. sft_assertEqual(5, size(layer%neurons)) &
                .and. sft_assertEqual(10, layer%neurons(0)%nextLayerSize)

        end function shouldCorrectlyInitInputLayer

end program layerTest
