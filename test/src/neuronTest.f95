program neuronTest
    use nt_TypesModule
    use nt_NeuronModule
    use SFT_Assert
    use SFT_Suit
    implicit none

    type(suit) :: testSuit
    call init(testSuit)
    
    call run(testSuit, shouldCorrectlyInitNeuron)

    call summary(testSuit)


    contains

        function shouldCorrectlyInitNeuron() result(res)
            use nt_TypesModule
            implicit none
            logical :: res
            type(nt_Neuron) :: neuron
            type(nt_Neuron) :: neuronWithBias

            call nt_neuronInit(neuron, 10, .False.)
            call nt_neuronInit(neuronWithBias, 10, .TRUE.)

            res = assertEqual(neuron%weightsSize, 10)
        end function




end program neuronTest
