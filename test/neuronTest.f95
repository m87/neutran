program neuronTest
    use SFT_Assert
    use SFT_Suit
    use neutran_Types
    use neutran_Neuron

    type(suit) :: testSuit
    call init(testSuit)
    
    call run(testSuit, shouldCorrectlyInitNeuron)

    call summary(testSuit)


    contains

        function shouldCorrectlyInitNeuron() result(res)
            logical :: res
            type(Neuron) :: neuron
            type(Neuron) :: neuronWithBias

            call neuron_init(neuron, 10, .False.)
            call neuron_init(neuronWithBias, 10, .TRUE.)

            res = assertEqual(neuron%weightsSize, 10)
        end function




end program neuronTest
