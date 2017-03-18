program neutranEx
    use nt_TypesModule
    use nt_NetModule
    use nt_FunctionsModule

    type(nt_Net) :: net

    call nt_netInit(nt_Net, (/ 2, 4, 1/))


end program neutranEx
