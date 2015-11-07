{
        currentLabel = "";
        if ($1 == "start")
        {
                currentLabel = $3;
        }
        else
        {
                currentLabel = $2;
        }

	print currentLabel;
}

#//at test3 | grep ShowTree | awk '{a = substr($1,match($1, /#/) + 1, 100); b = length(a) - 1; c = substr(a, 0, b); print c;}' | sort | uniq

