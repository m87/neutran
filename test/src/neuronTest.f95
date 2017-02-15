program neuronTest
    use nt_TypesModule
    use nt_NeuronModule
    use nt_initMethodsModule
    use sft_AssertModule
    use sft_SuiteModule
    implicit none

    type(sft_Suite) :: testSuite

    call sft_init(testSuite)
    
    call sft_run(testSuite, shouldCorrectlyInitHiddenNeuron)
    call sft_run(testSuite, shouldNotSetNextLayerForOutputNeuron)

    call sft_summary(testSuite)

    contains

        function shouldCorrectlyInitHiddenNeuron() result(res)
            logical :: res
            type(nt_Neuron) :: neuron

            call nt_hiddenNeuronInit(neuron, 10, mockWeightInitMethod, (/ 4.0 /))

            res = sft_assertEqual(size(neuron%synapses), 10)  & 
                .AND. sft_assertEqual(neuron%synapses(4)%weight, 4.0) 
                
        end function shouldCorrectlyInitHiddenNeuron

        function shouldNotSetNextLayerForOutputNeuron() result(res)
            logical :: res
            type(nt_Neuron) :: neuron

            call nt_outputNeuronInit(neuron)

            res = sft_assertEqual(size(neuron%synapses), 0) &
                .AND. sft_assertEqual(neuron%nextLayerSize, 0)
        end function shouldNotSetNextLayerForOutputNeuron

        subroutine mockWeightInitMethod(weight, args)
            real, intent(out) :: weight
            real, intent(in) :: args(0:)
        
            weight = args(0)

        end subroutine mockWeightInitMethod

end program neuronTest
