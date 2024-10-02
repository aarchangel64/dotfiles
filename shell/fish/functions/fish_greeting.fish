#function __pod_fortune
#    # make sure to generate the .dat file on modification by running strfile
#    fortune ~/dotfiles/resources/fortune/pod |
#        # Split text into multiple lines
#        fmt -w (math "round($COLUMNS * 0.75)") |
#        boxes -d square-brackets -a c -p h2v1 |
#        # Shift text to the right
#        nl -bn -w 12
#end

function fish_greeting
    set quote_width 0.75
    set image_pos 3 0

    clear

    # Get a fortune from custom NieR quotes file
    set fortune_output "$(fortune ~/dotfiles/resources/fortune/pod)"
    # Format the fortune text to 75% of terminal width
    set formatted_fortune "$(echo $fortune_output | fmt -w (math "round($COLUMNS * $quote_width)"))"
    # Add box around the formatted fortune
    set boxed_fortune "$(echo $formatted_fortune | boxes -d square-brackets -a c -p h2v1)"

    set _quote_height (echo $boxed_fortune | wc -l)
    set _quote_shift 2

    set image_support (test "$TERM_PROGRAM" = WezTerm; echo $status)

    if [ "$image_support" = 0 ]
        # 12.5% chance of showing pod the cat
        #if test (random 1 8) -eq 1
        if false
            set pod_img ~/dotfiles/resources/images/pod_cat.jpg
            #set pos 11x11@1x1
            #set width 176px
            set height (math $_quote_height - 2)
            #set pos "1,$(math round\(\($_quote_height - 3.5\) / 2\))"
            set filter lanczos3
        else
            set pod_img ~/dotfiles/resources/images/pod.png
            #set pos 7x7@2x0
            set height $_quote_height
            #set width 7
            set filter nearest
        end


        set test (identify -format "%w\n%h" $pod_img)
        set aspect (math $test[1] / $test[2])
        # Iosevka glyphs are exactly 1/2 em wide, so 1/0.5 = 2
        set font_aspect 2

        set _quote_shift (math ceil\($_quote_height x $aspect x $font_aspect\) + $image_pos[1] x 2)
    end

    # Shift text (all lines) to the right 
    set output "$(echo $boxed_fortune | nl -bn -w $_quote_shift)"
    echo $output

    #echo $test
    #echo h: $_quote_height
    #echo pos: $pos
    #echo aspect: $aspect
    #echo shift: $_quote_shift
    #echo $image_support

    if [ "$image_support" = 0 ]
        #wezterm imgcat $pod_img --width 8 --position 2,0
        #wezterm imgcat $pod_img --width $width --position $pos --resample-filter $filter
        wezterm imgcat $pod_img --height $height --position "$image_pos[1],$image_pos[2]" --resample-filter $filter
    end
end
