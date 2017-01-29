-module(fake_data).

-export([projects/0, project_by_path/1, filter_tags/0]).

projects() ->
    [
     [{path, "gar1t/mnist"},
      {description, "Models for MNIST digit recognition"},
      {name, "MNIST"},
      {account, "gar1t"},
      {updated, "Updated 3 minutes ago"},
      {tags, [tag("MNIST"), tag("CNN")]},
      {stars, 12},
      {fields, fake_fields()},
      {starred, true},
      {runs, 15}],
     [{path, "jjallaire/cifar10"},
      {description, "Models for CIFAR-10 image recognition"},
      {name, "CIFAR 10"},
      {account, "jjallaire"},
      {updated, "Updated on Dec 10, 2016"},
      {tags, [tag("CIFAR-10"), tag("CNN")]},
      {stars, 1},
      {runs, 10}],
     [{path, "jjallaire/mnist"},
      {description, "Models for MNIST digit recognition"},
      {name, "MNIST"},
      {account, "jjallaire"},
      {updated, "Updated 6 days ago"},
      {tags, [tag("MNIST")]},
      {stars, 5},
      {runs, 8}]
    ].

project_by_path(Path) ->
    project_by_path(Path, projects()).

project_by_path(Path, [P|Rest]) ->
    case proplists:get_value(path, P) of
        Path -> {ok, P};
        _ -> project_by_path(Path, Rest)
    end;
project_by_path(_Path, []) -> error.

tag(Name) ->
    [{name, Name}, {color, color_for_name(Name)}].

color_for_name(Name) ->
    case (erlang:phash2(Name, 3) + 1) of
        0 -> "#888";
        1 -> "rgb(82,135,198)";
        2 -> "rgb(48,163,141)";
        3 -> "rgb(197,182,102)"
    end.

filter_tags() ->
    [tag("Dataset", 36),
     tag("TensorFlow", 29),
     tag("Model", 27),
     tag("Resource", 25),
     tag("Paper", 23),
     tag("Images", 18),
     tag("Deep Learning", 12),
     tag("CNN", 10),
     tag("Music", 4),
     tag("ImageNet", 3),
     tag("AlexNet", 3),
     tag("LSTM", 3),
     tag("MLP", 2),
     tag("Video", 2),
     tag("Speech", 2),
     tag("Torch", 2),
     tag("Reinforcement", 2),
     tag("Classifier", 2),
     tag("Tutorial", 2),
     tag("Inception", 2),
     tag("Keras", 2),
     tag("Word2vec", 1),
     tag("DCGAN", 1),
     tag("Motion", 1),
     tag("Titanic", 1),
     tag("Scikit Flow", 1),
     tag("Scikit-learn", 1),
     tag("Software", 1),
     tag("Unsupervised", 1),
     tag("Multi GPU", 1),
     tag("Bayesian", 1),
     tag("Ratings", 1),
     tag("nlp", 1),
     tag("chatbot", 1),
     tag("seq2seq", 1),
     tag("Website", 1),
     tag("Tracking", 1),
     tag("Art", 1),
     tag("GRU", 1),
     tag("MR", 1),
     tag("Julia", 1),
     tag("Book", 1),
     tag("Spark", 1),
     tag("Supervised", 1),
     tag("Kaggle", 1)].

tag(Name, Count) ->
    [{id, id_for_name(Name)},
     {name, Name},
     {count, Count},
     {color, color_for_name(Name)}].

id_for_name(Name) ->
    erlang:phash2(Name, 10000000).

fake_fields() ->
    [
     [{color, "green-700"},
      {icon, "bullseye"},
      {caption, "Validation Accuracy"},
      {value, "90.74%"}],

     [{color, "teal-300"},
      {icon, "bullseye"},
      {caption, "Training Accuracy"},
      {value, "94.40%"}]
    ].
