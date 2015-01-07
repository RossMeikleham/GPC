;Compiler Generated TD file
(let 
    '(label hello_world 
        (begin 
            (let 
                '(assign y '"Hello World")
                '(apply display y))
            (if ('true) ; Need to reduce to: (if ('true) '(return '0) '(return '1)) 
                '(
                    '(return '0))
                '(
                    '(return '1))))))
(let 
    '(label hello_world_args (lambda 'a 'b 
        (begin 
            '(apply display '"Hello World")
            '(return '0))))) ; Can reduce to '0
'(apply hello_world)
'(apply hello_world_args '5 '5.4)
