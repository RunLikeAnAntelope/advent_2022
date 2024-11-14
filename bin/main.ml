let () =
    print_endline "Day one answers: ";  
    print_endline ("    " ^ (Solutions.One.answer1 "./resources/advent1.txt"));
    print_endline ("    " ^ (Solutions.One.answer2 "./resources/advent1.txt"));

    print_endline "Day two answers:";
    print_endline ("    " ^ Solutions.Two.answer1 "./resources/advent2.txt");
    print_endline ("    " ^ Solutions.Two.answer2 "./resources/advent2.txt");
